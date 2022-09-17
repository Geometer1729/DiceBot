module Main where

import Discord
import Discord.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Types

import Discord.Internal.Rest.Interactions as Rest

import Parser (parseRoll)
import RefTable (RefTable, maybeMakeRef, maybeUnRef, newRefTable)
import Roller (rollIO)

import Data.Text as T
import Expect (report)

main :: IO ()
main = do
  tok <- strip <$> readFileText "token.auth"
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
  ]

handler :: RefTable -> Event -> DiscordHandler ()
handler rt = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs = Prelude.filter (\c -> applicationCommandName c `notElem` (createName <$> coms)) oldComs
    forM_ removedComs $ rc_ . DeleteGlobalApplicationCommand i . applicationCommandId
    forM_ coms $ rc_ . CreateGlobalApplicationCommand i
    putStrLn "command registered"
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr)])
            }
        , ..
        }
    ) -> rollExpr rt interactionId interactionToken Nothing expr
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr), OptionDataValueInteger _ times'])
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
        (stripPrefix "roll:" -> Just expr) -> do
          rollExpr rt interactionId interactionToken Nothing expr
        (stripPrefix "rollt:" -> Just rest) -> do
          let (times', T.tail -> expr) = breakOn ":" rest
          case readMaybe $ toString times' of
            Just times -> do
              rollExpr rt interactionId interactionToken (Just times) expr
            Nothing -> die "failed to parse times in rollt"
        (stripPrefix "logs:" -> Just logs) -> do
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic logs
        (stripPrefix "stats:" -> Just rest) -> do
          let (res', T.tail -> expr) = breakOn "," rest
          roll <- case parseRoll expr of
            Left _ -> die "failed to  reparse in stats"
            Right r -> pure r
          res <- case readMaybe $ toString res' of
            Nothing -> die "faile to read res in stats"
            Just res -> pure res
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic $
                report roll res
        "ref:lost" ->
          rc_ $
            CreateInteractionResponse
              interactionId
              interactionToken
              $ interactionResponseBasic
                "sorry that content was lost on a bot restart"
        _ -> die $ toString $ "unexpected button data:" <> button
  e -> when False $ print e

rollExpr :: RefTable -> InteractionId -> InteractionToken -> Maybe Int -> Text -> DiscordHandler ()
rollExpr rt interactionId interactionToken times expr =
  case parseRoll expr of
    Left err ->
      rc_ $
        CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic $
            "Parsing: " <> expr <> "\nfailed with: " <> toText err
    Right roll -> do
      (res, logs) <- case times of
        Nothing -> first (show @Text) <$> rollIO roll
        Just t -> first show . mconcat . Prelude.map (first (pure @[])) <$> replicateM t (rollIO roll)
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
