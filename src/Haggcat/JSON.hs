module Haggcat.JSON where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           Haggcat.Types

decodeInstitutions :: LBS.ByteString -> Either String [Institution]
decodeInstitutions json = do
    insts <- Aeson.eitherDecode json
    return $ institution insts
