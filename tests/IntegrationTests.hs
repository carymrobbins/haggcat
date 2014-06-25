{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Data.ByteString                as BS
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit                     hiding (test)
import           Web.Authenticate.OAuth         as OAuth

import           Haggcat.Client
import           Haggcat.TestHelper

case_get_oauth_tokens = do
    client <- loadTestClient
    let cred = OAuth.unCredential . clientCredential $ client
        Just oToken = lookup "oauth_token" cred
        Just oSecret = lookup "oauth_token_secret" cred
    48 @=? BS.length oToken
    40 @=? BS.length oSecret

main = $(defaultMainGenerator)

