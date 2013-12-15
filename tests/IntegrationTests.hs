{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import System.Exit (exitFailure)

import Haggcat.Client
import Haggcat.TestHelper

tests :: Client -> [(String, IO ())]
tests client =
    [("test_loadClient", do
        userClient <- loadClient client
        return ()
    )]

main :: IO ()
main = do
    client <- getTestClient "test-files"
    mapM_ runTest . tests $ client

runTest :: (String, IO ()) -> IO ()
runTest (testName, test) = do
    putStrLn $ "*** Running Test: " ++ testName
    test

