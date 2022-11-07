{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Thanks where

import           Import          hiding (Env)
import           Util

import           Betitla.Display
import           Betitla.Env
import           Betitla.Error
import           Betitla.Striver

import           Witch           (from)

logIf :: MonadLogger m => Bool -> Text -> m ()
logIf True text = $(logInfo) text
logIf False _   = pure ()

getThanksR :: Handler Html
getThanksR = defaultLayout $ do
  rc            <- appEnv <$> getYesod
  maybeAuthCode <- lookupGetParam "code"
  authUrl       <- liftIO (getAuthUrl' rc <&> (++ "&approval_prompt=force"))
  scope         <- fromMaybe "No scope" <$> lookupGetParam "scope"
  let scopeOk    = hasRequiredScope scope
  let auth       = fromMaybe "No auth" maybeAuthCode
  let authOk     = isJust maybeAuthCode
  regResult     <- liftIO (if (scopeOk && authOk)
                    then (runReaderT (newUser $ AuthCode $ from auth) rc)
                    else pure $ Left $ StriveError "Could not get authorization")
  case regResult of
    Right aId -> do
      $(logInfo) $ "Registered new athlete: " ++ tshow aId
      setTitle "Blobfish thanks you" >> $(widgetFile "thanks")
    Left (BErrorNotNew _) ->
      $(logInfo) "An enthusiastic athlete attempted to register twice!" >>
      -- It is harmless to attempt to register/authenticate twice and doesn't
      -- seem unlikely so don't flag it, just politely thank the user like normal.
      setTitle "Blobfish thanks you" >> $(widgetFile "thanks")
    Left regError -> do
      $(logError) $ "Could not register new user.  Scope info: " ++ scope ++
                    "Auth code present: " ++ tshow authOk ++ " " ++
                    display regError
      setTitle "Blobfish is concerned"
      $(widgetFile "thanks-error")
