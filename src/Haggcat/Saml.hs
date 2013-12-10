{-# LANGUAGE OverloadedStrings #-}
module Haggcat.Saml where

import qualified Codec.Crypto.RSA as RSA
import Control.Applicative
import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.String.Utils (strip)
import Data.Time
import Data.UUID as U
import Data.UUID.V4 as U
import Numeric (showHex)
import System.Locale (defaultTimeLocale)
import Text.Regex

import Haggcat.Types

samlUrl :: String
samlUrl = "https://oauth.intuit.com/oauth/v1/get_access_token_by_saml"

type SamlAssertionId = LBS.ByteString
type SamlIssueInstant = UTCTime
type SamlNotBefore = UTCTime
type SamlNotOnOrAfter = UTCTime
type SamlSignature = LBS.ByteString

data Saml = Saml
    { samlAssertionId :: SamlAssertionId
    , samlIssueInstant :: SamlIssueInstant
    , samlNotBefore :: SamlNotBefore
    , samlNotOnOrAfter :: SamlNotOnOrAfter
    , samlSignature :: SamlSignature
    , samlIssuerId :: IssuerId
    , samlCustomerId :: CustomerId
    } deriving (Show)

newSaml :: IssuerId -> CustomerId -> IO Saml
newSaml issuerId customerId = do
    uuid <- newAssertionId
    now <- getCurrentTime
    return Saml
        { samlAssertionId=uuid
        , samlIssuerId=issuerId
        , samlIssueInstant=now
        , samlNotBefore=addUTCTime (-5 * 60) now
        , samlNotOnOrAfter=addUTCTime (10 * 60) now
        , samlCustomerId=customerId
        , samlSignature=""
        }

newAssertion :: Saml -> RSA.PrivateKey -> SamlAssertion
newAssertion saml privateKey = Base64.encode assertion
  where
    signedDigestValue = newSignedDigestValue saml
    signedSignatureValue =
        newSignedSignatureValue saml privateKey signedDigestValue
    signature = newSamlSignature
        (samlAssertionId saml)
        signedDigestValue
        signedSignatureValue
    assertion = newSamlAssertion $ saml { samlSignature=signature }



isoFormatTime :: UTCTime -> LBS.ByteString
isoFormatTime = LC.pack . formatTime defaultTimeLocale "%FT%T%QZ"

uuidToHex :: U.UUID -> LBS.ByteString
uuidToHex uuid = LC.pack $ concatMap (`showHex` "") [a,b,c,d]
  where
    (a,b,c,d) = U.toWords uuid

newAssertionId :: IO SamlAssertionId
newAssertionId = liftM uuidToHex nextRandom

newSignedSignatureValue
    :: Saml -> RSA.PrivateKey -> LBS.ByteString -> LBS.ByteString
newSignedSignatureValue saml privateKey signedDigestValue =
    Base64.encode . RSA.rsassa_pkcs1_v1_5_sign RSA.ha_SHA1 privateKey $
        newSamlSignedInfo
            (samlAssertionId saml)
            signedDigestValue

strictToLazyBS :: BS.ByteString -> LBS.ByteString
strictToLazyBS = LBS.fromChunks . pure

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS = BS.concat . LBS.toChunks

newSignedDigestValue :: Saml -> LBS.ByteString
newSignedDigestValue =
    Base64.encode . strictToLazyBS . SHA1.hashlazy . newSamlAssertion

newSamlAssertion :: Saml -> LBS.ByteString
newSamlAssertion saml =
    let assertionId = samlAssertionId saml
        isoNow = isoFormatTime . samlIssueInstant $ saml
        isoNotBefore = isoFormatTime . samlNotBefore $ saml
        isoNotAfter = isoFormatTime . samlNotOnOrAfter $ saml
        issuerId = samlIssuerId saml
        customerId = samlCustomerId saml
        signature = samlSignature saml
    in "\
\<saml2:Assertion xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"_" `LBS.append` assertionId `LBS.append` "\" IssueInstant=\"" `LBS.append` isoNow `LBS.append` "\" Version=\"2.0\">\
  \<saml2:Issuer>" `LBS.append` issuerId `LBS.append` "</saml2:Issuer>" `LBS.append` signature `LBS.append` "<saml2:Subject>\
    \<saml2:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\">" `LBS.append` customerId `LBS.append` "</saml2:NameID>\
    \<saml2:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"></saml2:SubjectConfirmation>\
  \</saml2:Subject>\
  \<saml2:Conditions NotBefore=\"" `LBS.append` isoNotBefore `LBS.append` "\" NotOnOrAfter=\"" `LBS.append` isoNotAfter `LBS.append` "\">\
    \<saml2:AudienceRestriction>\
      \<saml2:Audience>" `LBS.append` issuerId `LBS.append` "</saml2:Audience>\
    \</saml2:AudienceRestriction>\
  \</saml2:Conditions>\
  \<saml2:AuthnStatement AuthnInstant=\"" `LBS.append` isoNow `LBS.append` "\" SessionIndex=\"_" `LBS.append` assertionId `LBS.append` "\">\
    \<saml2:AuthnContext>\
      \<saml2:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:unspecified</saml2:AuthnContextClassRef>\
    \</saml2:AuthnContext>\
  \</saml2:AuthnStatement>\
\</saml2:Assertion>"

newSamlSignedInfo :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString
newSamlSignedInfo
    assertionId
    signedDigestValue = "\
\<ds:SignedInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">\
    \<ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></ds:CanonicalizationMethod>\
    \<ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"></ds:SignatureMethod>\
    \<ds:Reference URI=\"#_" `LBS.append` assertionId `LBS.append` "\">\
        \<ds:Transforms>\
            \<ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"></ds:Transform>\
            \<ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></ds:Transform>\
        \</ds:Transforms>\
        \<ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"></ds:DigestMethod>\
        \<ds:DigestValue>" `LBS.append` signedDigestValue `LBS.append` "</ds:DigestValue>\
    \</ds:Reference>\
\</ds:SignedInfo>"

newSamlSignature
    :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString
newSamlSignature
    assertionId
    signedDigestValue
    signedSignatureValue = "\
\<ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">\
    \<ds:SignedInfo>\
        \<ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\
        \<ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/>\
        \<ds:Reference URI=\"#_" `LBS.append` assertionId `LBS.append` "\">\
            \<ds:Transforms>\
                \<ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/>\
                \<ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\
            \</ds:Transforms>\
            \<ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/>\
                \<ds:DigestValue>" `LBS.append` signedDigestValue `LBS.append` "</ds:DigestValue>\
        \</ds:Reference>\
    \</ds:SignedInfo>\
    \<ds:SignatureValue>" `LBS.append` signedSignatureValue `LBS.append` "</ds:SignatureValue>\
\</ds:Signature>"

