module Main where

import Discord
import Discord.Interactions
import Discord.Types
import Discord.Internal.Rest.ApplicationCommands

import Discord.Internal.Rest.Interactions as Rest

import Parser(parseRoll)

import Data.Text (strip)
import Roller (rollIO)

main :: IO ()
main = do
  tok <- strip <$> readFileText "token.auth"
  print tok
  res <- runDiscord $
    def{ discordToken = tok
       , discordOnStart = startup
       , discordOnEvent = handler
       , discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPresences =True}
       }
  print res

startup :: DiscordHandler ()
startup = do
  liftIO $ putStrLn "Starting"

rc_ :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
rc_ = void . rc

rc :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
rc a = restCall a >>= \case
  Right r -> pure r
  Left err -> die $ show err

handler :: Event -> DiscordHandler ()
handler = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    let com =
          CreateApplicationCommandChatInput
          {createName="r"
          ,createLocalizedName=Nothing
          ,createDescription="roll some dice"
          ,createLocalizedDescription=Nothing
          ,createOptions=Just $ OptionsValues
            [ OptionValueString
                {optionValueName="expr"
                ,optionValueLocalizedName=Nothing
                ,optionValueDescription="the dice expression to be rolled"
                ,optionValueLocalizedDescription=Nothing
                ,optionValueRequired=True
                ,optionValueStringChoices = Left False
                ,optionValueStringMinLen=Just 1
                ,optionValueStringMaxLen=Nothing
                }
            ]
          ,createDefaultMemberPermissions=Nothing
          ,createDMPermission=Nothing
          }
    rc_ $ CreateGlobalApplicationCommand i com
    putStrLn "command registered"
  (InteractionCreate
    InteractionApplicationCommand
    {applicationCommandData = ApplicationCommandDataChatInput
      {optionsData = Just (OptionsDataValues [OptionDataValueString _ (Right expr)])
      }
    ,..
    }
   ) -> do
     case parseRoll expr of
       Left err ->
         rc_ $ CreateInteractionResponse
           interactionId
           interactionToken
           $ interactionResponseBasic $ "Asked to rol: " <> expr <> "\nparse failed with: "  <> toText err
       Right roll -> do
        (res,logs) <- liftIO $ rollIO roll
        rc_ $ CreateInteractionResponse
          interactionId
          interactionToken
          $ interactionResponseBasic $ "Asked to rol: " <> expr <> "\nrolled and got:" <> show res <> "\n" <> logs
  _ -> pass
