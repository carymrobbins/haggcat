{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Client where

import qualified Codec.Crypto.RSA           as RSA
import           Control.Monad              (ap)
import           Crypto.PubKey.OpenSsh
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      ()
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Conduit
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Network.HTTP.Conduit

import           Haggcat.Saml
import           Haggcat.Types

baseUrl :: String
baseUrl = "https://financialdatafeed.platform.intuit.com/rest-war/v1"

data Config = Config
    { consumerKey    :: ConsumerKey
    , consumerSecret :: ConsumerSecret
    , issuerId       :: IssuerId
    , customerId     :: CustomerId
    , privateKeyPath :: FilePath
    } deriving (Show, Read)

data Client = Client
    { clientConfig           :: Config
    , clientPrivateKey       :: RSA.PrivateKey
    , clientSaml             :: Saml
    , clientOAuthToken       :: OAuthToken
    , clientOAuthTokenSecret :: OAuthTokenSecret
    } deriving (Show)

type OAuthToken = LBS.ByteString
type OAuthTokenSecret = LBS.ByteString

loadClient :: Config -> IO Client
loadClient config = do
    pkey <- loadPrivateKey $ privateKeyPath config
    saml <- newSaml (issuerId config) (customerId config)
    let assertion = newAssertion saml pkey
    (oToken, oSecret) <- getOAuthTokens config assertion
    return $ Client
        { clientConfig=config
        , clientPrivateKey=pkey
        , clientSaml=saml
        , clientOAuthToken=oToken
        , clientOAuthTokenSecret=oSecret
        }

getOAuthTokens :: Config -> SamlAssertion -> IO (OAuthToken, OAuthTokenSecret)
getOAuthTokens client assertion = do
    manager <- newManager def
    initReq <- parseUrl samlUrl
    let headers = "OAuth oauth_consumer_key=\"" <> consumerKey client <> "\""
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

