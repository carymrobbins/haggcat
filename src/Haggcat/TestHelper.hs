module Haggcat.TestHelper where

import           Control.Monad
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           Haggcat.Client
import           Haggcat.Types

getTestConfig :: FilePath -> IO Config
getTestConfig = liftM read . readFile

loadTestClient :: IO Client
loadTestClient = loadClient =<< getTestConfig "test-files/config"

writeInstutions :: Client -> IO ()
writeInstutions client = do
    eitherInsts <- getInstitutions client
    case eitherInsts of
        Right insts -> writeFile "test-files/institutions" $ show insts
        Left msg -> error msg

loadInstitutions :: IO [Institution]
loadInstitutions = do
    content <- readFile "test-files/institutions"
    let insts = read content :: [Institution]
    return insts
