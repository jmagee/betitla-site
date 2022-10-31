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

import Debug.Trace as Debug(trace)

getWebhookR :: Handler Html
getWebhookR = defaultLayout $ do
  maybeWord      <- lookupGetParam "hub.verify_token"
  maybeHandshake <- lookupGetParam "hub.challenge"
  let _ = Debug.trace ("word = " ++ show maybeWord ++ ", handshake =" ++ show maybeHandshake)
  setTitle "Nothing to see here"
  $(widgetFile "webhook")
