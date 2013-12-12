{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Types where

import Data.ByteString as BS

type IssuerId = BS.ByteString
type ConsumerKey = BS.ByteString
type ConsumerSecret = BS.ByteString
type CustomerId = BS.ByteString
type ErrorMessage = String
type SamlAssertion = BS.ByteString

