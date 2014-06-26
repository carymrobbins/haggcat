{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Institution where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as LBS
import           GHC.Generics

import           Haggcat.JSON.Address (Address)
import           Haggcat.JSON.Decoder
import           Haggcat.JSON.Keys    (Keys)

instance DecodeContext Institution Institutions where
    unwrap = institution

data Institutions = Institutions { institution :: [Institution] }
    deriving (Show, Read, Generic)
instance Aeson.FromJSON Institutions
instance Aeson.ToJSON Institutions

data Institution = Institution { institutionId   :: Integer
                               , institutionName :: LBS.ByteString
                               , homeUrl         :: Maybe LBS.ByteString
                               , phoneNumber     :: Maybe LBS.ByteString
                               , virtual         :: Maybe Bool
                               -- Extra fields (maybe) retrieved when querying
                               -- institution details.
                               , status          :: Maybe LBS.ByteString
                               , address         :: Maybe Address
                               , emailAddress    :: Maybe LBS.ByteString
                               , specialText     :: Maybe LBS.ByteString
                               -- TODO: currencyCode can be defined in terms of an enum
                               -- http://www.xe.com/iso4217.php
                               , currencyCode    :: Maybe LBS.ByteString
                               , keys            :: Maybe Keys
                               } deriving (Show, Read, Generic)
instance Aeson.FromJSON Institution where
instance Aeson.ToJSON Institution where

