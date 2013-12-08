module Haggcat.Types where

import Data.ByteString.Lazy as LBS

type IssuerId = LBS.ByteString
type ConsumerKey = LBS.ByteString
type ConsumerSecret = LBS.ByteString
type CustomerId = LBS.ByteString
type ErrorMessage = String

