{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Data.ByteString.Lazy as LBS
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (test)

import Haggcat.Client
import Haggcat.TestHelper

case_get_oauth_tokens = do
    client <- getTestClient "test-files"
    userClient <- loadClient client
    48 @=? (LBS.length $ userOAuthToken userClient)
    40 @=? (LBS.length $ userOAuthTokenSecret userClient)

main = $(defaultMainGenerator)

