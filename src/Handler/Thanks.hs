{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Thanks where

import           Import hiding (Env)
import           Util

import           Betitla.Env
import           Betitla.Striver

import Witch (into)

logIf :: MonadLogger m => Bool -> Text -> m ()
logIf True text = $(logInfo) text
logIf False _   = pure ()

getThanksR :: Handler Html
getThanksR = defaultLayout $ do
  rc          <- appEnv <$> getYesod
  maybeScope  <- lookupGetParam "scope"
  maybeAuthCode <- lookupGetParam "code"
  --auth        <- pure ((withReaderT appEnv getAuthUrl'') <&> (++ "&approval_prompt=force"))
  auth        <- liftIO (getAuthUrl' rc <&> (++ "&approval_prompt=force"))
  let scope   = maybe "No scope" id maybeScope
  let scopeOk = hasRequiredScope scope
  logIf (not scopeOk) $ "Missing required scope.  Got " ++ scope
  if scopeOk
    then case maybeAuthCode of
      Nothing -> $(logError) "No authorization code"
      Just a  -> liftIO (processNewUser rc $ AuthCode (into a)) >>= $(logInfo)
    else $(logInfo) $ "Missing required scope.  Got " ++ scope

  setTitle "Blobfish thanks you"
  $(widgetFile "thanks")

processNewUser :: Env -> AuthCode -> IO Text
processNewUser env auth = (into . show) <$> runReaderT (newUser auth) env
