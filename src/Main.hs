module Main where

import Discord
import Discord.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Types

import Discord.Internal.Rest.Interactions as Rest

import Parser (parseRoll)
import RefTable (RefTable, maybeMakeRef, maybeUnRef, newRefTable)
import Roller (rollIO)

import Data.Text qualified as T
import Expect (report)
import Flow((.>))
import Control.Arrow (ArrowChoice(left, right))

main :: IO ()
main = do
  tok <- T.strip <$> readFileText "token.auth"
  rt <- newRefTable
  print
    =<< runDiscord
      def
        { discordToken = tok
        , discordOnStart = startup
        , discordOnEvent = handler rt
        , discordGatewayIntent =
            def
              { gatewayIntentMembers = True
              , gatewayIntentPresences = True
              }
        }

startup :: DiscordHandler ()
startup = do
  liftIO $ putStrLn "Starting"

coms :: [CreateApplicationCommand]
coms =
  [ CreateApplicationCommandChatInput
      { createName = "r"
      , createLocalizedName = Nothing
      , createDescription = "roll some dice"
      , createLocalizedDescription = Nothing
      , createOptions =
          Just $
            OptionsValues
              [ OptionValueString
                  { optionValueName = "expr"
                  , optionValueLocalizedName = Nothing
                  , optionValueDescription = "the dice expression to be rolled"
                  , optionValueLocalizedDescription = Nothing
                  , optionValueRequired = True
                  , optionValueStringChoices = Left False
                  , optionValueStringMinLen = Just 1
                  , optionValueStringMaxLen = Nothing
                  }
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
      , createDefaultMemberPermissions = Nothing
      , createDMPermission = Nothing
      }
  , CreateApplicationCommandChatInput
      { createName = "help"
      , createLocalizedName = Nothing
      , createDescription = "send help text"
      , createLocalizedDescription = Nothing
      , createOptions = Nothing
      , createDefaultMemberPermissions = Nothing
      , createDMPermission = Nothing
      }
  ]

handler :: RefTable -> Event -> DiscordHandler ()
handler rt = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs = Prelude.filter (\c -> applicationCommandName c `notElem` (createName <$> coms)) oldComs
    forM_ removedComs $ rc_ . DeleteGlobalApplicationCommand i . applicationCommandId
    forM_ coms $ rc_ . CreateGlobalApplicationCommand i
    putStrLn "commands registered"
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { applicationCommandDataName = "help"
            }
        , ..
        }
    ) -> rc_ $ CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic
          $ "/help prints this\n"
          <> "/r rolls an expression\n"
          <> helpText
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { applicationCommandDataName = "r"
            , optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr)])
            }
        , ..
        }
    ) -> rollExpr rt interactionId interactionToken Nothing expr
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { applicationCommandDataName = "r"
            , optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr), OptionDataValueInteger _ times'])
            }
        , ..
        }
    ) ->
      let times = case times' of
            Left _ -> Nothing
            Right t -> Just $ fromInteger t
       in rollExpr rt interactionId interactionToken times expr
  ( InteractionCreate
      InteractionComponent
        { interactionId
        , interactionToken
        , componentData = ButtonData button
        }
    ) ->
      maybeUnRef rt button >>= \case
        (T.stripPrefix "roll:" -> Just expr) -> do
          rollExpr rt interactionId interactionToken Nothing expr
        (T.stripPrefix "rollt:" -> Just rest) -> do
          let (times', T.tail -> expr) = T.breakOn ":" rest
          case readMaybe $ toString times' of
            Just times -> do
              rollExpr rt interactionId interactionToken (Just times) expr
            Nothing -> die "failed to parse times in rollt"
        (T.stripPrefix "logs:" -> Just logs) -> do
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic logs
        (T.stripPrefix "stats:" -> Just rest) -> do
          let (res', T.tail -> expr) = T.breakOn "," rest
          roll <- case parseRoll expr of
            Left _ -> die "failed to  reparse in stats"
            Right r -> pure r
          res <- case readMaybe $ toString res' of
            Nothing -> die "failed to read res in stats"
            Just res -> pure res
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic $
                report roll res
        (T.stripPrefix "err:" -> Just msg) ->
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic
              msg
        _ -> die $ toString $ "unexpected button data:" <> button
  e -> when False $ print e

helpText :: Text
helpText
  = "expressions can be:\n"
  <> "constants like 1,3 or 42\n"
  <> "math like 1+1 or 2\\*3\n"
  <> "dice like d20,d12 or d6\n"
  <> "multiple dice like 5d6 or 2d8\n"
  <> "dice can also be mixed with math like 2*(3d12+d6+6)\n"
  <> "dice can also have rerolls\n"
  <> "like d20r2k1 to roll a d20 twice and keep the best 1\n"
  <> "or d6r5kw1 to roll a d20 twice and keep the worst 1\n"
  <> "or d6ru3 to roll a d6 but reroll low rolls up to 3\n"
  <> "or d6rou3 to roll a d6 and reroll low rolls up to 3 but only once\n"
  <> "the numbers of dice can also be expresions\n"
  <> "like (4+2)d6\n"
  <> "or (d3)d(d3)\n"


rollExpr :: RefTable -> InteractionId -> InteractionToken -> Maybe Int -> Text -> DiscordHandler ()
rollExpr rt interactionId interactionToken times expr =
  case parseRoll expr of
    Left _ ->
      rc_ $
        CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic $
            "Failed to parse: " <> expr <> "\n\n" <> helpText
    Right roll -> do
      (res' :: Either Text (Text,Text)) <- case times of
        Nothing -> rollIO roll <&> right (first (show @Text))
        Just t -> replicateM t (rollIO roll) <&> (sequence .> right (unzip .> second T.concat .> first (show @Text)))
      case res' of
        Left err -> rc_
          $ CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic err
        Right (res :: Text,logs) -> do
          let rollPrefix =
                case times of
                  Nothing -> "roll:"
                  Just t -> "rollt:" <> show t <> ":"
          logMsg <- maybeMakeRef rt ("logs:" <> (if logs == "" then "It was a constant." else logs))
          rollMsg <- maybeMakeRef rt (rollPrefix <> expr)
          statsMsg <- maybeMakeRef rt ("stats:" <> res <> "," <> expr)
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ InteractionResponseChannelMessage $
                InteractionResponseMessage
                  { interactionResponseMessageTTS = Nothing
                  , interactionResponseMessageContent =
                      Just $ expr <> "= **" <> res <> "**"
                  , interactionResponseMessageEmbeds = Nothing
                  , interactionResponseMessageAllowedMentions = Nothing
                  , interactionResponseMessageFlags = Nothing
                  , interactionResponseMessageComponents =
                      Just
                        [ ActionRowButtons
                            [ Button
                                { buttonCustomId = rollMsg
                                , buttonDisabled = False
                                , buttonStyle = ButtonStylePrimary
                                , buttonLabel = Just "Reroll"
                                , buttonEmoji = Nothing
                                }
                            , Button
                                { buttonCustomId = logMsg
                                , buttonDisabled = False
                                , buttonStyle = ButtonStylePrimary
                                , buttonLabel = Just "How?"
                                , buttonEmoji = Nothing
                                }
                            , Button
                                { buttonCustomId = statsMsg
                                , buttonDisabled = False
                                , buttonStyle = ButtonStylePrimary
                                , buttonLabel = Just "Stats"
                                , buttonEmoji = Nothing
                                }
                            ]
                        ]
                  , interactionResponseMessageAttachments = Nothing
                  }

rc_ :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
rc_ = void . rc

rc :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
rc a =
  restCall a >>= \case
    Right r -> pure r
    Left err -> die $ show err
