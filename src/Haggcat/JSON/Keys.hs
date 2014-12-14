{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Keys where

import qualified Data.Aeson      as Aeson
import           Data.Text       (Text)
import           GHC.Generics

data Keys = Keys { key :: [Key] } deriving (Show, Read, Generic)
instance Aeson.FromJSON Keys
instance Aeson.ToJSON Keys

data Key = Key { name           :: Text
               , val            :: Maybe Text
               , status         :: Text
               , valueLengthMin :: Int
               , valueLengthMax :: Int
               , displayFlag    :: Bool
               , displayOrder   :: Int
               , mask           :: Bool
               , instructions   :: Text
               , description    :: Text
               } deriving (Show, Read, Generic)
instance Aeson.FromJSON Key
instance Aeson.ToJSON Key

