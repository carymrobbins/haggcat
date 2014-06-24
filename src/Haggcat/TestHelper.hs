module Haggcat.TestHelper where

import Control.Monad

import Haggcat.Client

getTestConfig :: FilePath -> IO Config
getTestConfig = liftM read . readFile

loadTestClient :: IO Client
loadTestClient = loadClient =<< getTestConfig "test-files/config"
