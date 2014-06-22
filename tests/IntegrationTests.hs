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
    client <- loadClient =<< getTestConfig "test-files/config"
    48 @=? (LBS.length $ clientOAuthToken client)
    40 @=? (LBS.length $ clientOAuthTokenSecret client)

main = $(defaultMainGenerator)

