module Main where

import Control.Arrow (right)
import Data.Text qualified as T
import Discord
import Discord.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Types
import Flow ((.>))
import Parser (parseRoll)
import RefTable (RefTable, maybeMakeRef, maybeUnRef, newRefTable)
import Response (Response, followUp, mkInteractionHandler, rc, rc_, respond)
import Sample (rollIO)
import Stats (genReport)

main :: IO ()
main = do
  token <- T.strip <$> readFileText "./token.auth"
  rt <- newRefTable
  print
    =<< runDiscord
      def
        { discordToken = token
        , discordOnStart = liftIO $ putStrLn "Starting"
        , discordOnEvent = handler rt
        , discordGatewayIntent =
            def
              { gatewayIntentMembers = True
              , gatewayIntentPresences = True
              }
        }

coms :: [CreateApplicationCommand]
coms =
  [ simpleCommand "r" "roll some dice" $
      Just $
        OptionsValues
          [ exprOption
          , OptionValueInteger
              { optionValueName = "times"
              , optionValueLocalizedName = Nothing
              , optionValueDescription = "time number of times to roll it"
              , optionValueLocalizedDescription = Nothing
              , optionValueRequired = False
              , optionValueIntegerChoices = Left False
              , optionValueIntegerMinVal = Just 1
              , optionValueIntegerMaxVal = Nothing
              }
          ]
  , simpleCommand "stats" "get stats info" $
      Just $
        OptionsValues
          [ exprOption
          , OptionValueInteger
              { optionValueName = "result"
              , optionValueLocalizedName = Nothing
              , optionValueDescription = "the result"
              , optionValueLocalizedDescription = Nothing
              , optionValueRequired = True
              , optionValueIntegerChoices = Left False
              , optionValueIntegerMinVal = Nothing
              , optionValueIntegerMaxVal = Nothing
              }
          ]
  , simpleCommand "help" "send help text" Nothing
  ]

handler :: RefTable -> Event -> DiscordHandler ()
handler rt = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs =
          Prelude.filter
            (\c -> applicationCommandName c `notElem` (createName <$> coms))
            oldComs
    forM_ removedComs $ rc_ . DeleteGlobalApplicationCommand i . applicationCommandId
    forM_ coms $ rc . CreateGlobalApplicationCommand i
    putStrLn "commands registered"
  InteractionCreate interaction ->
    mkInteractionHandler interaction $
      case interaction of
        ( InteractionApplicationCommand
            { applicationCommandData =
              ApplicationCommandDataChatInput
                { applicationCommandDataName = name
                , optionsData = options
                }
            }
          ) ->
            case name of
              "help" ->
                respond $
                  interactionResponseBasic $
                    "/help prints this\n"
                      <> "/r rolls an expression\n"
                      <> helpText
              "stats" ->
                case options of
                  ( Just
                      ( OptionsDataValues
                          [ OptionDataValueString _ (Right expr)
                            , OptionDataValueInteger _ (Right result)
                            ]
                        )
                    ) -> stats (fromInteger result) expr
                  _ -> putStrLn $ "Bad options for stats:" <> show options
              "r" ->
                case options of
                  ( Just
                      ( OptionsDataValues
                          [OptionDataValueString _ (Right expr)]
                        )
                    ) -> rollExpr rt Nothing expr
                  ( Just
                      ( OptionsDataValues
                          [ OptionDataValueString _ (Right expr)
                            , OptionDataValueInteger _ (Right times)
                            ]
                        )
                    ) -> rollExpr rt (Just $ fromInteger times) expr
                  _ -> putStrLn $ "Bad options for r: " <> show options
              com -> putStrLn $ "bad command: " <> show com
        (InteractionComponent {componentData = ButtonData button}) ->
          maybeUnRef rt button >>= \case
            (T.stripPrefix "roll:" -> Just expr) -> do
              rollExpr rt Nothing expr
            (T.stripPrefix "rollt:" -> Just rest) -> do
              let (times', T.tail -> expr) = T.breakOn ":" rest
              case readMaybe $ toString times' of
                Just times -> do
                  rollExpr rt (Just times) expr
                Nothing -> die "failed to parse times in rollt"
            (T.stripPrefix "logs:" -> Just logs) -> do
              respond $ interactionResponseBasic logs
            (T.stripPrefix "stats:" -> Just rest) -> do
              let (res', T.tail -> expr) = T.breakOn "," rest
              res <- case readMaybe $ toString res' of
                Nothing -> die "failed to read res in stats"
                Just res -> pure res
              stats res expr
            (T.stripPrefix "err:" -> Just msg) ->
              respond $
                interactionResponseBasic
                  msg
            _ -> die $ toString $ "unexpected button data:" <> button
        i -> do
          putStrLn "unhandled interaction"
          print i
  _ -> pass

helpText :: Text
helpText =
  "expressions can be:\n"
    <> "constants like 1,3 or 42\n"
    <> "math like 1+1 or 2\\*3\n"
    <> "dice like d20,d12 or d6\n"
    <> "multiple dice like 5d6 or 2d8\n"
    <> "dice can also be mixed with math like 2*(3d12+d6+6)\n"
    <> "dice can also have rerolls\n"
    <> "like `d20 reroll 2 keep best 1` (which can be shortened to d20r2k1) to roll a d20 twice and keep the best 1\n"
    <> "or `d6 reroll 5 keep worst 1` (shortened to d6r5kw1) to roll a d20 twice and keep the worst 1\n"
    <> "or `d6 reroll up to 3` (shortened to d6ru3) to roll a d6 but reroll low rolls up to 3\n"
    <> "or `d6 reroll once up to 3` (shortened to d6rou3) to roll a d6 and reroll low rolls up to 3 but at most once\n"
    <> "the numbers of dice can also be expresions\n"
    <> "like (4+2)d6\n"
    <> "or (d3)d(d3)\n"
    <> "there is also special syntax for song of swords checks"
    <> "sos5 will roll 5d10s and return the number of them which were at least 7"
    <> "if the target is not 7 it can be specified after a t like sos5t6"

rollExpr :: RefTable -> Maybe Int -> Text -> Response ()
rollExpr rt times expr =
  case parseRoll expr of
    Left _ ->
      respond $
        interactionResponseBasic $
          "Failed to parse: " <> expr <> "\n\n" <> helpText
    Right roll -> do
      (res' :: Either Text (Text, Text)) <- case times of
        Nothing -> rollIO roll <&> right (first (show @Text))
        Just t -> replicateM t (rollIO roll) <&> (sequence .> right (unzip .> second T.concat .> first (show @Text)))
      case res' of
        Left err ->
          respond $ interactionResponseBasic err
        Right (res :: Text, logs) -> do
          let rollPrefix =
                case times of
                  Nothing -> "roll:"
                  Just t -> "rollt:" <> show t <> ":"
          buttons <-
            mapM
              (uncurry $ genButton rt)
              [ ("Reroll", rollPrefix <> expr)
              ,
                ( "How?"
                , "logs:"
                    <> (if logs == "" then "It was a constant." else logs)
                )
              , ("Stats", "stats:" <> res <> "," <> expr)
              ]
          respond $
            InteractionResponseChannelMessage $
              InteractionResponseMessage
                { interactionResponseMessageTTS = Nothing
                , interactionResponseMessageContent =
                    Just $ expr <> "= **" <> res <> "**"
                , interactionResponseMessageEmbeds = Nothing
                , interactionResponseMessageAllowedMentions = Nothing
                , interactionResponseMessageFlags = Nothing
                , interactionResponseMessageComponents =
                    Just [ActionRowButtons buttons]
                , interactionResponseMessageAttachments = Nothing
                }

stats :: Int -> Text -> Response ()
stats res expr = do
  roll <- case parseRoll expr of
    Left _ -> die "failed to  reparse in stats"
    Right r -> pure r
  genReport roll res >>= \case
    Right report ->
      respond $ interactionResponseBasic report
    Left cont -> do
      void $ respond InteractionResponseDeferChannelMessage
      liftIO cont >>= \case
        Just report ->
          followUp $ interactionResponseMessageBasic report
        Nothing ->
          followUp $ interactionResponseMessageBasic "sorry timed out"

simpleButton :: Text -> Text -> Button
simpleButton buttonId label =
  Button
    { buttonCustomId = buttonId
    , buttonDisabled = False
    , buttonStyle = ButtonStylePrimary
    , buttonLabel = Just label
    , buttonEmoji = Nothing
    }

genButton :: MonadIO m => RefTable -> Text -> Text -> m Button
genButton rt label msg = do
  msg' <- maybeMakeRef rt msg
  pure $ simpleButton msg' label

simpleCommand :: Text -> Text -> Maybe Options -> CreateApplicationCommand
simpleCommand name desc opts =
  CreateApplicationCommandChatInput
    { createName = name
    , createLocalizedName = Nothing
    , createDescription = desc
    , createLocalizedDescription = Nothing
    , createOptions = opts
    , createDefaultMemberPermissions = Nothing
    , createDMPermission = Nothing
    }

exprOption :: OptionValue
exprOption =
  OptionValueString
    { optionValueName = "expr"
    , optionValueLocalizedName = Nothing
    , optionValueDescription = "the dice expression to be rolled"
    , optionValueLocalizedDescription = Nothing
    , optionValueRequired = True
    , optionValueStringChoices = Left False
    , optionValueStringMinLen = Just 1
    , optionValueStringMaxLen = Nothing
    }
