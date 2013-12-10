{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Client where

import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString as BS
import qualified Data.ByteString as LBS
import Network.HTTP.Conduit

import Haggcat.Saml
import Haggcat.Types

data Client = Client
    { consumerKey :: ConsumerKey
    , consumerSecret :: ConsumerSecret
    , issuerId :: IssuerId
    , customerId :: CustomerId
    , privateKeyPath :: FilePath
    }

data UserClient = UserClient
    { userClient :: Client
    , userPrivateKey :: RSA.PrivateKey
    , userSaml :: Saml
    , userOAuthToken :: OAuthToken
    , userOAuthTokenSecret :: OAuthTokenSecret
    }

type OAuthToken = LBS.ByteString
type OAuthTokenSecret = LBS.ByteString

loadClient :: Client -> IO UserClient
loadClient client = do
    pkey <- loadPrivateKey $ privateKeyPath client
    saml <- newSaml (issuerId client) (customerId client)
    assertion <- newAssertion saml pkey
    (oToken, oSecret) <- getOAuthTokens client assertion
    return $ UserClient
        { userClient=client
        , userPrivateKey=pkey
        , userSaml=saml
        , userOAuthToken=oToken
        , userOAuthTokenSecret=oSecret
        }

getOAuthTokens :: Client -> SamlAssertion -> IO (OAuthToken, OAuthTokenSecret)
getOAuthTokens client assertion = do
    manager <- newManager def
    initReq <- parseUrl samlUrl
    let auth_headers = "OAuth oauth_consumer_key=" `LBS.append`
                       consumerKey client
    let body = urlEncodedBody [("saml_assertion", lazyToStrictBS assertion)]
    let req = body $ initReq { method="POST"
                             , requestHeaders=[("Authorization", auth_headers)]
                             }
    response <- runResourceT $ httpLbs req manager
    return . parseBody . responseBody $ response

parseBody :: LC.ByteString -> [(LC.ByteString, LC.ByteString)]
parseBody = fmap ((fmap (LC.drop 1)) . LC.break (=='=')) . LC.split '&'

loadPrivateKey :: FilePath -> IO RSA.PrivateKey
loadPrivateKey path = (throwLeft . decodePrivate) `fmap` BS.readFile p

throwLeft :: Either String OpenSshPrivateKey -> RSA.PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s

