{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Keys where

import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as LBS
import           GHC.Generics

data Keys = Keys { key :: [Key] } deriving (Show, Read, Generic)
instance Aeson.FromJSON Keys
instance Aeson.ToJSON Keys

data Key = Key { name           :: LBS.ByteString
               , val            :: Maybe LBS.ByteString
               , status         :: LBS.ByteString
               , valueLengthMin :: Int
               , valueLengthMax :: Int
               , displayFlag    :: Bool
               , displayOrder   :: Int
               , mask           :: Bool
               , instructions   :: LBS.ByteString
               , description    :: LBS.ByteString
               } deriving (Show, Read, Generic)
instance Aeson.FromJSON Key
instance Aeson.ToJSON Key

