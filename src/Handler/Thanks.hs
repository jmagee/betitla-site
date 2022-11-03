{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Thanks where

import           Import
import           Util

import           Betitla.Striver

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
  setTitle "Blobfish thanks you"
  $(widgetFile "thanks")

--processNewUser :: AuthCode -> IO Text
--processNewUser auth
