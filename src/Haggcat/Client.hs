{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Client where

import qualified Codec.Crypto.RSA as RSA
import Control.Monad (ap)
import Crypto.PubKey.OpenSsh
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Conduit
import Data.Maybe
import Network.HTTP.Conduit

import Haggcat.Classes
import Haggcat.Saml
import Haggcat.Types

baseUrl :: String
baseUrl = "https://financialdatafeed.platform.intuit.com/rest-war/v1"

data Client = Client
    { consumerKey :: ConsumerKey
    , consumerSecret :: ConsumerSecret
    , issuerId :: IssuerId
    , customerId :: CustomerId
    , privateKeyPath :: FilePath
    } deriving (Show)

data UserClient = UserClient
    { userClient :: Client
    , userPrivateKey :: RSA.PrivateKey
    , userSaml :: Saml
    , userOAuthToken :: OAuthToken
    , userOAuthTokenSecret :: OAuthTokenSecret
    } deriving (Show)

type OAuthToken = LBS.ByteString
type OAuthTokenSecret = LBS.ByteString

loadClient :: Client -> IO UserClient
loadClient client = do
    pkey <- loadPrivateKey $ privateKeyPath client
    saml <- newSaml (issuerId client) (customerId client)
    let assertion = newAssertion saml pkey
    (oToken, oSecret) <- getOAuthTokens client assertion
    return $ UserClient
        { userClient=client
        , userPrivateKey=pkey
        , userSaml=saml
        , userOAuthToken=oToken
        , userOAuthTokenSecret=oSecret
        }

getOAuthTokens
    :: Client -> SamlAssertion -> IO (OAuthToken, OAuthTokenSecret)
getOAuthTokens client assertion = do
    manager <- newManager def
    initReq <- parseUrl samlUrl
    let headers = "OAuth oauth_consumer_key=\"" <+> consumerKey client <+> "\""
    let body = [("saml_assertion", assertion)]
    let req = urlEncodedBody body $ initReq
                { method="POST"
                , requestHeaders=[("Authorization", headers)]
                }
    response <- runResourceT $ httpLbs req manager
    let result = parseBody . responseBody $ response
    let get k = lookup k result
    let maybeTokens = Just (,) `ap` get "oauth_token"
                               `ap` get "oauth_token_secret"
    return $ fromMaybe
        (error $ "Tokens not found in response: " ++ show result)
        maybeTokens

parseBody :: LBS.ByteString -> [(LBS.ByteString, LBS.ByteString)]
parseBody = fmap ((fmap (LC.drop 1)) . LC.break (=='=')) . LC.split '&'

loadPrivateKey :: FilePath -> IO RSA.PrivateKey
loadPrivateKey p = (throwLeft . decodePrivate) `fmap` BS.readFile p

throwLeft :: Either String OpenSshPrivateKey -> RSA.PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s

