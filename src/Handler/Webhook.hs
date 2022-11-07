{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Webhook where

import           Import

import           Betitla.Display
import           Betitla.Env
import           Betitla.Striver

import Control.Lens.Getter ((^.))
import           Control.Monad.Reader (runReaderT)
import           Witch                (from)
import Data.Text (Text)
import Strive (SubscriptionEvent (..), objectType)

import Debug.Trace as Debug(trace)

getWebhookR :: Handler Value
getWebhookR = do
  maybeWord      <- lookupGetParam "hub.verify_token"
  maybeHandshake <- lookupGetParam "hub.challenge"
  case maybeHandshake of
    Nothing -> pure $ object ["hub.challenge" .= ("invalid" :: Text)]
    Just x  -> pure $ object ["hub.challenge" .= x]

postWebhookR :: Handler Value
postWebhookR = do
  --postParams  <- getPostParams
  -- $(logInfo) $ from $ show postParams
  jsonBody    <- requireJsonBody :: Handler SubscriptionEvent
  case (jsonBody ^. objectType) of
    "activity" -> $(logInfo) "activity event"
    "athlete"  -> $(logInfo) "athlete event"
    x          -> $(logWarn) $"unknown event, ignoring: " ++ x
  $(logInfo) $ from $ show jsonBody
  pure $ object ["result" .= ("ok" :: Text)]
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
