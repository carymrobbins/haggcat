module Haggcat.Types
    ( module Haggcat.Types
    , module Haggcat.JSON.Institution
    ) where

import qualified Data.ByteString      as BS

import           Haggcat.JSON.Institution (Institution)

type IssuerId = BS.ByteString
type ConsumerKey = BS.ByteString
type ConsumerSecret = BS.ByteString
type CustomerId = BS.ByteString
type ErrorMessage = String
type SamlAssertion = BS.ByteString

