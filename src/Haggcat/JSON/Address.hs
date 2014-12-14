{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Address where

import qualified Data.Aeson      as Aeson
import           Data.Text       (Text)
import           GHC.Generics

data Address = Address { address1   :: Maybe Text
                       , address2   :: Maybe Text
                       , address3   :: Maybe Text
                       , city       :: Maybe Text
                       , state      :: Maybe Text
                       , postalCode :: Maybe Int
                       , country    :: Maybe Text
                       } deriving (Show, Read, Generic)
instance Aeson.FromJSON Address
instance Aeson.ToJSON Address

