{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON.Institution where

import qualified Data.Aeson           as Aeson
import           Data.Text            (Text)
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
                               , institutionName :: Text
                               , homeUrl         :: Maybe Text
                               , phoneNumber     :: Maybe Text
                               , virtual         :: Maybe Bool
                               -- Extra fields (maybe) retrieved when querying
                               -- institution details.
                               , status          :: Maybe Text
                               , address         :: Maybe Address
                               , emailAddress    :: Maybe Text
                               , specialText     :: Maybe Text
                               -- TODO: currencyCode can be defined in terms of an enum
                               -- http://www.xe.com/iso4217.php
                               , currencyCode    :: Maybe Text
                               , keys            :: Maybe Keys
                               } deriving (Show, Read, Generic)
instance Aeson.FromJSON Institution where
instance Aeson.ToJSON Institution where

