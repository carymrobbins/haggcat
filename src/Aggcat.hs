{-# LANGUAGE OverloadedStrings #-}
module Aggcat where

import qualified Codec.Crypto.RSA as RSA
import Control.Exception
import Control.Monad
import Crypto.PubKey.OpenSsh
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import Data.Conduit
import Data.String.Utils
import Network.HTTP.Conduit
import System.IO

import Aggcat.Types
import Aggcat.Saml

generateAssertion = do
    let customerId = "2"
    issuerId <- getIssuerId
    saml <- newSaml issuerId customerId
    eitherPkOrError <- getCertificate
    return $ assert saml eitherPkOrError
  where
    assert saml (Right pk) = Right $ newAssertion saml pk
    assert _ (Left e) = Left e

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
    eAssertion <- generateAssertion
    getOAuthTokens eAssertion

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

getCertificate :: IO (Either ErrorMessage RSA.PrivateKey)
getCertificate = do
    liftM (extract . decodePrivate) $ BS.readFile "test-files/certificate.key"
  where
    extract (Right (OpenSshPrivateKeyRsa pkey)) = Right pkey
    extract (Right _) = Left "Private key must be RSA, not DSA."
    extract (Left e) = (Left e)


getConsumerKey :: IO ConsumerKey
getConsumerKey = readKey "consumer_key"

getConsumerSecret :: IO ConsumerSecret
getConsumerSecret = readKey "consumer_secret"

getIssuerId :: IO IssuerId
getIssuerId = readKey "issuer_id"

