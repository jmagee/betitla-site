{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import Import

import Betitla.Display
import Betitla.Env
import Betitla.Striver

import Control.Monad.Reader (runReaderT)
import Witch (from)

getAuthUrl' :: IO Text
getAuthUrl' = getEnvRC >>= \env -> runReaderT getAuthUrl env >>= \case
  Left e  -> pure $ "There was an error.  Blobfish says: " ++ display e
  Right x -> pure $ from x

getHomeR:: Handler Html
getHomeR= defaultLayout $ do
  authUrl <- liftIO getAuthUrl'
  setTitle "Betitla"
  $(widgetFile "betitla")
