{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Thanks where

import           Import

import           Betitla.Striver (hasRequiredScope)

getThanksR :: Handler Html
getThanksR = defaultLayout $ do
  maybeScope  <- lookupGetParam "scope"
  let scope   = maybe "No scope" id maybeScope
  let scopeOk = hasRequiredScope scope
  setTitle "Blobfish thanks you"
  $(widgetFile "thanks")
