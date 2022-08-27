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

main :: IO ()
main = do
  tok <- strip <$> readFileText "token.auth"
  rt <- newRefTable
  print tok
  res <-
    runDiscord $
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
  print res

startup :: DiscordHandler ()
startup = do
  liftIO $ putStrLn "Starting"

rc_ :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
rc_ = void . rc

rc :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
rc a =
  restCall a >>= \case
    Right r -> pure r
    Left err -> die $ show err

handler :: RefTable -> Event -> DiscordHandler ()
handler rt = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    let com =
          CreateApplicationCommandChatInput
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
                    ]
            , createDefaultMemberPermissions = Nothing
            , createDMPermission = Nothing
            }
    rc_ $ CreateGlobalApplicationCommand i com
    putStrLn "command registered"
  ( InteractionCreate
      InteractionApplicationCommand
        { applicationCommandData =
          ApplicationCommandDataChatInput
            { optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr)])
            }
        , ..
        }
    ) -> rollExpr rt interactionId interactionToken expr
  ( InteractionCreate
      InteractionComponent
        { interactionId
        , interactionToken
        , componentData = ButtonData button
        }
    ) -> case button of
      (stripPrefix "roll:" -> Just rest) -> do
        expr <- maybeUnRef rt rest
        rollExpr rt interactionId interactionToken expr
      (stripPrefix "logs:" -> Just rest) -> do
        logs <- maybeUnRef rt rest
        rc_ $
          CreateInteractionResponse
            interactionId
            interactionToken
            $ interactionResponseBasic logs
      _ -> die $ toString $ "unexpected button data:" <> button
  _ -> pass

rollExpr :: RefTable -> InteractionId -> InteractionToken -> Text -> DiscordHandler ()
rollExpr rt interactionId interactionToken expr =
  case parseRoll expr of
    Left err ->
      rc_ $
        CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic $ "Parsing: " <> expr <> "\nfailed with: " <> toText err
    Right roll -> do
      (res, logs) <- rollIO roll
      logMsg <- maybeMakeRef rt (100 - T.length "logs:") logs
      exprRef <- maybeMakeRef rt (100 - T.length "roll:") expr
      rc_ $
        CreateInteractionResponse
          interactionId
          interactionToken
          $ InteractionResponseChannelMessage $
            InteractionResponseMessage
              { interactionResponseMessageTTS = Nothing
              , interactionResponseMessageContent =
                  Just $
                    expr <> "= **" <> show res <> "**"
              , interactionResponseMessageEmbeds = Nothing
              , interactionResponseMessageAllowedMentions = Nothing
              , interactionResponseMessageFlags = Nothing
              , interactionResponseMessageComponents =
                  Just
                    [ ActionRowButtons
                        [ Button
                            { buttonCustomId = "roll:" <> exprRef
                            , buttonDisabled = False
                            , buttonStyle = ButtonStylePrimary
                            , buttonLabel = Just "reroll"
                            , buttonEmoji = Nothing
                            }
                        , Button
                            { buttonCustomId = "logs:" <> logMsg
                            , buttonDisabled = False
                            , buttonStyle = ButtonStylePrimary
                            , buttonLabel = Just "How?"
                            , buttonEmoji = Nothing
                            }
                        ]
                    ]
              , interactionResponseMessageAttachments = Nothing
              }
