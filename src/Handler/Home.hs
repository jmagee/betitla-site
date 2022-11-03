{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import hiding (Env)
import           Util

import           Betitla.Display
import           Betitla.Env
import           Betitla.Striver

import           Witch                (from)

{-getAuthUrl' :: Env -> IO Text-}
{-getAuthUrl' = runReaderT getAuthUrl >=> \case-}
  {-Left e  -> pure $ "There was an error.  Blobfish says: " ++ display e-}
  {-Right x -> pure $ from x-}

getHomeR:: Handler Html
getHomeR= defaultLayout $ do
  rc      <- appEnv <$> getYesod
  authUrl <- liftIO $ getAuthUrl' rc
  setTitle "Betitla"
  $(widgetFile "betitla")
