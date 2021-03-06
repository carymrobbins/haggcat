{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Client where

import qualified Codec.Crypto.RSA           as RSA
import           Control.Applicative        ((<$>))
import           Control.Monad              (ap)
import           Control.Monad.Trans.Resource (runResourceT)
import           Crypto.PubKey.OpenSsh
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      ()
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Conduit
import qualified Web.Authenticate.OAuth as OAuth

import           Haggcat.JSON.Decoder
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
    , clientOAuth            :: OAuth.OAuth
    , clientCredential       :: OAuth.Credential
    } deriving (Show)

type OAuthToken = LBS.ByteString
type OAuthTokenSecret = LBS.ByteString

loadClient :: Config -> IO Client
loadClient config = do
    pkey <- loadPrivateKey $ privateKeyPath config
    saml <- newSaml (issuerId config) (customerId config)
    let assertion = newAssertion saml pkey
    (oToken, oSecret) <- getOAuthTokens config assertion
    let oauth = OAuth.newOAuth { OAuth.oauthServerName="oauth.intuit.com"
                               , OAuth.oauthConsumerKey=consumerKey config
                               , OAuth.oauthConsumerSecret=consumerSecret config
                               }
    let cred = OAuth.newCredential (LBS.toStrict oToken) (LBS.toStrict oSecret)
    return Client
        { clientConfig=config
        , clientPrivateKey=pkey
        , clientSaml=saml
        , clientOAuth=oauth
        , clientCredential=cred
        }

getOAuthTokens :: Config -> SamlAssertion -> IO (OAuthToken, OAuthTokenSecret)
getOAuthTokens config assertion = do
    manager <- newManager tlsManagerSettings
    initReq <- parseUrl samlUrl
    let headers = "OAuth oauth_consumer_key=\"" <> consumerKey config <> "\""
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
        -- TODO: Better error handling, possibly use Either.
        (error $ "Tokens not found in response: " ++ show result)
        maybeTokens

makeRequest urlPath httpMethod body headers client = do
    -- TODO: Refactor me!
    initReq <- parseUrl $ baseUrl <> "/" <> urlPath
    let applyBody = if httpMethod == "POST" then urlEncodedBody body else id
    let req = applyBody $ initReq
                { method=httpMethod
                , responseTimeout=Nothing
                , requestHeaders=[ ("Content-Type", "application/json")
                                 , ("Accept", "application/json")
                                 ] ++ headers
                }
    res <- withManager $ \m -> do
        signedreq <- OAuth.signOAuth (clientOAuth client) (clientCredential client) req
        httpLbs signedreq m
    return $ responseBody res

requestAndDecode urlPath httpMethod body headers client =
    decodeResponse <$> makeRequest urlPath httpMethod body headers client

getAccounts :: Client -> IO LBS.ByteString
getAccounts = makeRequest "accounts" "GET" [] []

getInstitutions :: Client -> IO (Either String [Institution])
getInstitutions = requestAndDecode "institutions" "GET" [] []

getInstitutionDetails :: String -> Client -> IO (Either String Institution)
getInstitutionDetails instId client = do
    res <- makeRequest ("institutions/" <> instId) "GET" [] [] client
    return $ Aeson.eitherDecode res

parseBody :: LBS.ByteString -> [(LBS.ByteString, LBS.ByteString)]
parseBody = fmap (fmap (LC.drop 1) . LC.break (=='=')) . LC.split '&'

loadPrivateKey :: FilePath -> IO RSA.PrivateKey
loadPrivateKey p = (throwLeft . decodePrivate) `fmap` BS.readFile p

throwLeft :: Either String OpenSshPrivateKey -> RSA.PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s
