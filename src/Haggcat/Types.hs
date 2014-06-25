{-# LANGUAGE DeriveGeneric #-}
module Haggcat.Types where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics

type IssuerId = BS.ByteString
type ConsumerKey = BS.ByteString
type ConsumerSecret = BS.ByteString
type CustomerId = BS.ByteString
type ErrorMessage = String
type SamlAssertion = BS.ByteString

data Institution = Institution { institutionId   :: Integer
                               , institutionName :: LBS.ByteString
                               , homeUrl         :: Maybe LBS.ByteString
                               , phoneNumber     :: Maybe LBS.ByteString
                               , virtual         :: Bool
                               } deriving (Show, Read, Generic)
instance Aeson.FromJSON Institution where
instance Aeson.ToJSON Institution where

data InstitutionContainer = InstitutionContainer { institution :: [Institution] }
    deriving (Show, Read, Generic)
instance Aeson.FromJSON InstitutionContainer
instance Aeson.ToJSON InstitutionContainer
