{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Thanks where

import           Import

import Betitla.Display
import Betitla.Env
import Betitla.Striver

import Control.Monad.Reader (runReaderT)
import Witch (from)

--import           Betitla.Striver (hasRequiredScope)

hasRequiredScope :: Text -> Bool
hasRequiredScope text = isInfixOf "activity:read" text &&
                        isInfixOf "activity:wrzte" text

-- Fixme
getAuthUrl' :: IO Text
getAuthUrl' = getEnvRC >>= \env -> runReaderT getAuthUrl env >>= \case
  Left e  -> pure $ "There was an error.  Blobfish says: " ++ display e
  Right x -> pure $ from x

getThanksR :: Handler Html
getThanksR = defaultLayout $ do
  maybeScope  <- lookupGetParam "scope"
  auth        <- liftIO (getAuthUrl' <&> (++ "&approval_prompt=force"))
  let scope   = maybe "No scope" id maybeScope
  let scopeOk = hasRequiredScope scope
  setTitle "Blobfish thanks you"
  $(widgetFile "thanks")
