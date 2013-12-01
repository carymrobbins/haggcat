{-# LANGUAGE OverloadedStrings #-}
module Aggcat where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as C
import Data.Conduit
import Data.String.Utils
import Network.HTTP.Conduit
import Network.TLS.Extra
import System.IO

import Aggcat.Types
import Aggcat.Saml

intuit = do
    consumerKey <- getConsumerKey
    manager <- newManager def
    initReq <- parseUrl "https://oauth.intuit.com/oauth/v1/get_access_token_by_saml"
    let req = initReq { method = "POST"
                      , requestHeaders =
                          [("Authorization",
                            C.pack $ "OAuth oauth_consumer_key=" ++ show consumerKey)]
                      , requestBody = RequestBodyBS "foo" --samlAssertion 
                      }
    resp <- runResourceT $ httpLbs req manager
    return resp

putStrFlush :: String -> IO ()
putStrFlush x = putStr x >> hFlush stdout

getUsername :: IO String
getUsername = putStrFlush "Username: " >> getLine

getPassword = do
    putStrFlush "Password: "
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

keyDirectory :: String
keyDirectory = "/home/crobbins/projects/haskell-aggcat/test-files/"

readKey :: String -> IO String
readKey = liftM strip . readFile . (keyDirectory ++)

--getCertificate :: IO PrivateKey
getCertificate = fileReadPrivateKey $ "test-files/certificate.key"

getConsumerKey :: IO ConsumerKey
getConsumerKey = readKey "consumer_key"

getConsumerSecret :: IO ConsumerSecret
getConsumerSecret = readKey "consumer_secret"

getIssuerId :: IO IssuerId
getIssuerId = readKey "issuer_id"

