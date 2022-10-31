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

import           Control.Monad.Reader (runReaderT)
import           Witch                (from)
import Data.Text (Text)

import Debug.Trace as Debug(trace)

getWebhookR :: Handler Value
getWebhookR = do
  maybeWord      <- lookupGetParam "hub.verify_token"
  maybeHandshake <- lookupGetParam "hub.challenge"
  case maybeHandshake of
    Nothing -> pure $ object ["hub.challenge" .= ("invalid" :: Text)]
    Just x  -> pure $ object ["hub.challenge" .= x]

{-getWebhookR :: Handler Html-}
{-getWebhookR = defaultLayout $ do-}
  {-maybeWord      <- lookupGetParam "hub.verify_token"-}
  {-maybeHandshake <- lookupGetParam "hub.challenge"-}
  {-let _ = Debug.trace ("word = " ++ show maybeWord ++ ", handshake =" ++ show maybeHandshake)-}
  {-setTitle "Nothing to see here"-}
  {-$(widgetFile "webhook")-}
