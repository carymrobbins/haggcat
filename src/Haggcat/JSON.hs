{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haggcat.JSON where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           Haggcat.Types

class (Aeson.FromJSON b) => DecodeContext a b | a -> b where
    unwrap :: b -> [a]

    decodeResponse :: LBS.ByteString -> Either String [a]
    decodeResponse = fmap unwrap . Aeson.eitherDecode

instance DecodeContext Institution InstitutionContainer where
    unwrap = institution
