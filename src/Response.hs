module Response (
  Response,
  respond,
  followUp,
  mkInteractionHandler,
  rc,
  rc_,
) where

import Discord (DiscordHandler, FromJSON, Request, restCall)
import Discord.Interactions (Interaction (..), InteractionResponse, InteractionResponseMessage)
import Discord.Requests (InteractionResponseRequest (..))
import Discord.Types (ApplicationId, InteractionId, InteractionToken)

newtype Response a
  = Response
      (ReaderT Info DiscordHandler a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

data Info = Info
  { infoInteractionId :: InteractionId
  , infoInteractionToken :: InteractionToken
  , infoInteractionApplicationId :: ApplicationId
  }

mkInteractionHandler :: Interaction -> Response () -> DiscordHandler ()
mkInteractionHandler interaction (Response r) =
  runReaderT
    r
    Info
      { infoInteractionId = interactionId interaction
      , infoInteractionToken = interactionToken interaction
      , infoInteractionApplicationId = interactionApplicationId interaction
      }

respond :: InteractionResponse -> Response ()
respond ir = Response $ do
  Info {..} <- ask
  lift $
    rc_ $
      CreateInteractionResponse
        infoInteractionId
        infoInteractionToken
        ir

followUp :: InteractionResponseMessage -> Response ()
followUp irm = Response $ do
  Info {..} <- ask
  lift $
    rc_ $
      CreateFollowupInteractionMessage
        infoInteractionApplicationId
        infoInteractionToken
        irm

rc_ :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
rc_ = void . rc

rc :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
rc a =
  restCall a >>= \case
    Right r -> pure r
    Left err -> die $ show err
