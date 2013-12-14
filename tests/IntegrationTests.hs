{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import System.Exit (exitFailure)

import Haggcat.Client

tests :: Client -> [(String, IO ())]
tests client =
    [("test_loadClient", do
        userClient <- loadClient client
        return ()
    )]

getClient :: IO Client
getClient = liftM5 Client
    (readTestData "test-files/consumerKey")
    (readTestData "test-files/consumerSecret")
    (readTestData "test-files/issuerId")
    (readTestData "test-files/customerId")
    (return       "test-files/certificate.key")

readTestData :: String -> IO C.ByteString
readTestData = liftM (C.pack . strip) . readFile

main :: IO ()
main = do
    client <- getClient
    mapM_ runTest . tests $ client

runTest :: (String, IO ()) -> IO ()
runTest (testName, test) = do
    putStrLn $ "*** Running Test: " ++ testName
    test

