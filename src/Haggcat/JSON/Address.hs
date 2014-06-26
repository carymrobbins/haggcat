{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Address where

import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as LBS
import           GHC.Generics

data Address = Address { address1   :: Maybe LBS.ByteString
                       , address2   :: Maybe LBS.ByteString
                       , address3   :: Maybe LBS.ByteString
                       , city       :: Maybe LBS.ByteString
                       , state      :: Maybe LBS.ByteString
                       , postalCode :: Maybe Int
                       , country    :: Maybe LBS.ByteString
                       } deriving (Show, Read, Generic)
instance Aeson.FromJSON Address
instance Aeson.ToJSON Address

