{-# LANGUAGE OverloadedStrings #-}
module Haggcat where

import qualified Codec.Crypto.RSA as RSA
import Control.Exception
import Control.Monad
import Crypto.PubKey.OpenSsh
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Conduit
import Data.String.Utils
import Network.HTTP.Conduit
import System.IO

import Haggcat.Types
import Haggcat.Saml

getR (Right x) = x

generateSaml = do
    let customerId = "2"
    issuerId <- getIssuerId
    newSaml issuerId customerId

generateAssertion = do
    let customerId = "2"
    issuerId <- getIssuerId
    saml <- newSaml issuerId customerId
    pkey <- getCertificate
    return $ newAssertion saml pkey

getOAuthTokens (Left e) = return $ Left e
getOAuthTokens (Right assertion) = do
    consumerKey <- getConsumerKey
    manager <- newManager def
    initReq <- parseUrl "https://oauth.intuit.com/oauth/v1/get_access_token_by_saml"
    let req = urlEncodedBody [("saml_assertion", lazyToStrictBS assertion)] $ initReq
                { method = "POST"
                , requestHeaders =
                    [("Authorization",
                      C.pack $ "OAuth oauth_consumer_key=" ++ show consumerKey)]
                }
    response <- runResourceT $ httpLbs req manager
    return $ Right response

tryGetOAuthTokens = do
    assertion <- generateAssertion
    getOAuthTokens $ Right assertion

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
keyDirectory = "test-files/"

readKey :: String -> IO LBS.ByteString
readKey = liftM (LC.pack . strip) . readFile . (keyDirectory ++)


throwLeft :: Either String OpenSshPrivateKey -> RSA.PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s

loadKey :: FilePath -> IO RSA.PrivateKey
loadKey p = (throwLeft . decodePrivate) `fmap` BS.readFile p

getCertificate :: IO RSA.PrivateKey
getCertificate = loadKey "test-files/certificate.key"

getConsumerKey :: IO ConsumerKey
getConsumerKey = readKey "consumer_key"

getConsumerSecret :: IO ConsumerSecret
getConsumerSecret = readKey "consumer_secret"

getIssuerId :: IO IssuerId
getIssuerId = readKey "issuer_id"

