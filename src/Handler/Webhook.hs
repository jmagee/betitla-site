{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Webhook
( getWebhookR
, postWebhookR
) where

import           Import

import           Betitla.Display
import           Betitla.Env
import           Betitla.Striver

import Data.Function ((&))
import Control.Lens.Getter ((^.))
import           Control.Monad.Reader (runReaderT)
import           Witch                (from, unsafeInto)
import Data.Text (Text)
import Strive (SubscriptionEvent (..), objectType, objectId, aspectType, ownerId, updates, authorized)

import Debug.Trace as Debug(trace)

getWebhookR :: Handler Value
getWebhookR = do
  maybeWord      <- lookupGetParam "hub.verify_token"
  maybeHandshake <- lookupGetParam "hub.challenge"
  case maybeHandshake of
    Nothing -> pure $ object ["hub.challenge" .= ("invalid" :: Text)]
    Just x  -> pure $ object ["hub.challenge" .= x]

handleActivityEvent :: SubscriptionEvent -> Handler Bool
handleActivityEvent event =
  let activityId = ActivityId $ unsafeInto @Int64 (event ^. objectId)
      aspect     = event ^. aspectType
      athleteId  = event ^. ownerId
  in do
    $(logInfo) $ "Handling activity = " ++ tshow activityId ++
                 " with aspect = " ++ aspect ++
                 " and athelete = " ++ tshow athleteId
    case aspect of
      "create" -> pure True
      x        -> $(logInfo) "We only rename on activity creations"
               >> pure False

handleAthleteEvent :: SubscriptionEvent -> Handler Bool
handleAthleteEvent event =
  let aspect    = event ^. aspectType
      athleteId = AthleteId $ unsafeInto @Int64 (event ^. ownerId)
  in do
    $(logInfo) $ "Handling athlete = " ++ tshow athleteId ++
                 " with aspect = " ++ aspect
    case aspect of
      x        -> pure False
      "update" -> -- believe it or not, the revoke event is update not delete
        let deauth = event ^. updates >>= (^. authorized)
        in (deauth == Just "false") & bool (pure False) (do
          env <- appEnv <$> getYesod
          r <- liftIO $ runReaderT (removeUser athleteId) env
          case r of
            Left e  -> $(logError) (display e) >> pure False
            Right _ -> $(logInfo) ("Removed " ++ tshow athleteId) >> pure True)

postWebhookR :: Handler Value
postWebhookR = do
  --postParams  <- getPostParams
  -- $(logInfo) $ from $ show postParams
  jsonBody    <- requireJsonBody :: Handler SubscriptionEvent
  $(logInfo) $ from $ show jsonBody
  case jsonBody ^. objectType of
    "activity" -> do
      $(logInfo) "activity event"
      ok <- handleActivityEvent jsonBody
      pure $ object ["result" .= (ok :: Bool)]

    "athlete"  -> do
      $(logInfo) "athlete event"
      ok <- handleAthleteEvent jsonBody
      pure $ object ["result" .= ("ok" :: Text)]

    x          -> do
      $(logWarn) $"unknown event, ignoring: " ++ x
      pure $ object ["result" .= ("ignore" :: Text)]
