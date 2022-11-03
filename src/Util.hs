{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
( getAuthUrl'
) where

import           Import hiding (Env)

import           Betitla.Display
import           Betitla.Env
import           Betitla.Striver

import           Witch           (from)

getAuthUrl' :: Env -> IO Text
getAuthUrl' env = runReaderT getAuthUrl env <&>
  either (("There was an error.  Blobfish says: " ++) . display)
         (from)
