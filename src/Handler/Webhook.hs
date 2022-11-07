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

import Control.Lens.Getter ((^.))
import           Control.Monad.Reader (runReaderT)
import           Witch                (from, unsafeInto)
import Data.Text (Text)
import Strive (SubscriptionEvent (..), objectType, objectId, aspectType, ownerId)

import Debug.Trace as Debug(trace)

getWebhookR :: Handler Value
getWebhookR = do
  maybeWord      <- lookupGetParam "hub.verify_token"
  maybeHandshake <- lookupGetParam "hub.challenge"
  case maybeHandshake of
    Nothing -> pure $ object ["hub.challenge" .= ("invalid" :: Text)]
    Just x  -> pure $ object ["hub.challenge" .= x]

handleActivityEvent :: SubscriptionEvent -> Handler Bool
handleActivityEvent  event =
  let activityId = ActivityId $ unsafeInto @Int64 (event ^. objectId)
      aspect     = event ^. aspectType
      athleteId  = event ^. ownerId
  in do
    $(logInfo) $ "Handling activity = " ++ tshow activityId ++
                 " with aspect = " ++ aspect ++
                 " and athelete = " ++ tshow athleteId
    pure True

postWebhookR :: Handler Value
postWebhookR = do
  --postParams  <- getPostParams
  -- $(logInfo) $ from $ show postParams
  jsonBody    <- requireJsonBody :: Handler SubscriptionEvent
  $(logInfo) $ from $ show jsonBody
  case (jsonBody ^. objectType) of
    "activity" -> do
      $(logInfo) "activity event"
      ok <- handleActivityEvent jsonBody
      pure $ object ["result" .= (ok :: Bool)]

    "athlete"  -> do
      $(logInfo) "athlete event"
      pure $ object ["result" .= ("ok" :: Text)]

    x          -> do
      $(logWarn) $"unknown event, ignoring: " ++ x
      pure $ object ["result" .= ("ignore" :: Text)]
  {-maybeType   <- lookupPostParam "object_type"-}
  --maybeId     <- lookupPostParam "object_id"
  {-maybeApsect <- lookupPostParam "aspect_type"-}
  {-maybeOwner  <- lookupPostParam "owner_id"-}
  {-$(logInfo) $ "Post webhook request received for " ++ from (show maybeOwner)-}
  {-$(logInfo) $ "Post webhook request received for " ++ from (show maybeApsect)-}
  {-$(logInfo) $ "Post webhook request received for " ++ from (show maybeId)-}
  {-$(logInfo) $ "Post webhook request received for " ++ from (show maybeType)-}
  --setTitle "Nothing to see here"
  -- $(widgetFile "webhook")

{-postHomeR :: Handler Html-}
{-postHomeR = do-}
    {-((result, formWidget), formEnctype) <- runFormPost sampleForm-}
    {-let handlerName = "postHomeR" :: Text-}
        {-submission = case result of-}
            {-FormSuccess res -> Just res-}
            {-_ -> Nothing-}

    {-defaultLayout $ do-}
        {-let (commentFormId, commentTextareaId, commentListId) = commentIds-}
        {-aDomId <- newIdent-}
        {-setTitle "Welcome To Yesod!"-}
        {-$(widgetFile "homepage")-}
{-getWebhookR :: Handler Html-}
{-getWebhookR = defaultLayout $ do-}
  {-maybeWord      <- lookupGetParam "hub.verify_token"-}
  {-maybeHandshake <- lookupGetParam "hub.challenge"-}
  {-let _ = Debug.trace ("word = " ++ show maybeWord ++ ", handshake =" ++ show maybeHandshake)-}
  {-setTitle "Nothing to see here"-}
  {-$(widgetFile "webhook")-}
