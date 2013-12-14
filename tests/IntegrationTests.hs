{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import System.Exit (exitFailure)

import Haggcat.Client

-- To run the tests, create a test-files directory in the root of
-- this project.  Create files containing consumerKey, consumerSecret,
-- issuerId, customerId, and certificate.key

getClient :: IO Client
getClient = do
    consumerKey <- readTestData "consumerKey"
    consumerSecret <- readTestData "consumerSecret"
    issuerId <- readTestData "issuerId"
    customerId <- readTestData "customerId"
    return Client
        { consumerKey=consumerKey
        , consumerSecret=consumerSecret
        , issuerId=issuerId
        , customerId=customerId
        , privateKeyPath="test-files/certificate.key"
        }

readTestData :: String -> IO C.ByteString
readTestData = liftM (C.pack . strip) . readFile . ("test-files/" ++)

main :: IO ()
main = do
    client <- getClient
    mapM_ runTest . tests $ client

runTest :: (String, IO ()) -> IO ()
runTest (testName, test) = do
    putStrLn $ "*** Running Test: " ++ testName
    test

tests :: Client -> [(String, IO ())]
tests client =
    [("test_loadClient", do
        userClient <- loadClient client
        return ()
    )]

