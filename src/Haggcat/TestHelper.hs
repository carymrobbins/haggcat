module Haggcat.TestHelper where

import Control.Monad

import Haggcat.Client

getTestConfig :: FilePath -> IO Config
getTestConfig path = liftM read $ readFile path
