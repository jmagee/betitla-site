{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Goodbye
( getGoodbyeR
) where

import           Import          hiding (Env)

readBool :: Text -> Bool
readBool "True" = True
readBool      _ = False

getGoodbyeR :: Handler Html
getGoodbyeR = defaultLayout $ do
  maybeOut <- lookupGetParam "deauthenticate"
  case maybeOut of
    Nothing -> getPage False
    Just x  -> getPage $ readBool x
  where
    getPage out@True  = setTitle "So long and thanks for all the blobfish" >> $(widgetFile "goodbye")
    getPage out@False = setTitle "Are you sure you want to go?" >> $(widgetFile "goodbye")
