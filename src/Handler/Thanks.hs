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
  rc            <- appEnv <$> getYesod
  maybeScope    <- lookupGetParam "scope"
  maybeAuthCode <- lookupGetParam "code"
  authUrl       <- liftIO (getAuthUrl' rc <&> (++ "&approval_prompt=force"))
  let scope   = fromMaybe "No scope" maybeScope
  let scopeOk = hasRequiredScope scope
  let auth    = fromMaybe "No auth" maybeAuthCode
  let authOk  = isJust maybeAuthCode
  if scopeOk && authOk
    then liftIO (processNewUser rc $ AuthCode (into auth)) >>= $(logInfo)
    else $(logInfo) $ "Missing required scope.  Got " ++ scope

  setTitle "Blobfish thanks you"
  $(widgetFile "thanks")

{-register :: (Bool, Bool) -> Text -> Text -> Env -> IO Text-}
{-register (True, True) scope auth env = processNewUser env (AuthCode (into auth) >>= \case-}
  {-Left e -> do -}
    {-$(logError) $ display e -}
    {-pure "Unfortunately there was a problem and we could not complete your registeration."-}
  {-Right token -> --}
    {-$(logInfo) token-}

{-register (True, False) _ _ _ = pure "The data we obtained from Strava was malformed.  Try again?"-}
{-register (False, _) scope _ _ = pure "Missing required scope.  Got " ++ scope"-}

processNewUser :: Env -> AuthCode -> IO Text
processNewUser env auth = (into . show) <$> runReaderT (newUser auth) env
