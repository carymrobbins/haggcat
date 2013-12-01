{-# LANGUAGE OverloadedStrings #-}
module Aggcat.Saml where

import qualified Codec.Crypto.RSA as RSA
import Control.Applicative
import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import Data.Time
import Data.UUID as U
import Data.UUID.V4 as U
import Numeric
import System.Locale (defaultTimeLocale)
import Text.Regex

import Aggcat.Types

type SamlAssertionId = String
type SamlIssueInstant = UTCTime
type SamlNotBefore = UTCTime
type SamlNotOnOrAfter = UTCTime
type SamlSignature = String

data Saml = Saml
    { samlAssertionId :: SamlAssertionId
    , samlIssueInstant :: SamlIssueInstant
    , samlNotBefore :: SamlNotBefore
    , samlNotOnOrAfter :: SamlNotOnOrAfter
    , samlSignature :: SamlSignature
    , samlIssuerId :: IssuerId
    , samlCustomerId :: CustomerId
    } deriving (Show)

isoFormatTime :: UTCTime -> String
isoFormatTime = formatTime defaultTimeLocale "%FT%T%QZ"

uuidToHex :: U.UUID -> String
uuidToHex uuid = concatMap (`showHex` "") [a,b,c,d]
  where
    (a,b,c,d) = U.toWords uuid

newAssertionId :: IO SamlAssertionId
newAssertionId = liftM uuidToHex nextRandom

cleanSaml :: String -> String
cleanSaml = strip . flip (subRegex (mkRegex ">\\s*<")) "><"

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

newSignedSignatureValue :: Saml -> RSA.PrivateKey -> LBS.ByteString
newSignedSignatureValue saml privateKey =
    Base64.encode . RSA.sign privateKey . strictToLazyBS . SHA1.hash . C.pack $ newSamlSignedInfo
        (samlAssertionId saml)
        (LC.unpack . newSignedDigestValue $ saml)

strictToLazyBS :: BS.ByteString -> LBS.ByteString
strictToLazyBS = LBS.fromChunks . pure

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS = BS.concat . LBS.toChunks

newSignedDigestValue :: Saml -> LBS.ByteString
newSignedDigestValue saml =
    Base64.encode . strictToLazyBS . SHA1.hash . C.pack $ assertion
  where
    assertion = newSamlAssertion
        (samlAssertionId saml)
        (isoFormatTime . samlIssueInstant $ saml)
        (isoFormatTime . samlNotBefore $ saml)
        (isoFormatTime . samlNotOnOrAfter $ saml)
        (samlIssuerId saml)
        (samlSignature saml)
        (samlCustomerId saml)

newSamlAssertion
  :: String
     -> String
     -> String
     -> String
     -> String
     -> String
     -> String
     -> String
newSamlAssertion
    assertionId
    isoNow
    isoNotBefore
    isoNotAfter
    issuerId
    signature
    customerId = cleanSaml "\
\<saml2:Assertion xmlns:saml2=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"_%" ++ assertionId ++ "\" IssueInstant=\"" ++ isoNow ++ "\" Version=\"2.0\">\
  \<saml2:Issuer>" ++ issuerId ++ "</saml2:Issuer>" ++ signature ++ "<saml2:Subject>\
    \<saml2:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\">" ++ customerId ++ "</saml2:NameID>\
    \<saml2:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"></saml2:SubjectConfirmation>\
  \</saml2:Subject>\
  \<saml2:Conditions NotBefore=\"" ++ isoNotBefore ++ "\" NotOnOrAfter=\"" ++ isoNotAfter ++ "\">\
    \<saml2:AudienceRestriction>\
      \<saml2:Audience>" ++ issuerId ++ "</saml2:Audience>\
    \</saml2:AudienceRestriction>\
  \</saml2:Conditions>\
  \<saml2:AuthnStatement AuthnInstant=\"" ++ isoNow ++ "\" SessionIndex=\"_" ++ assertionId ++ "\">\
    \<saml2:AuthnContext>\
      \<saml2:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:unspecified</saml2:AuthnContextClassRef>\
    \</saml2:AuthnContext>\
  \</saml2:AuthnStatement>\
\</saml2:Assertion>"

newSamlSignedInfo :: String -> String -> String
newSamlSignedInfo
    assertionId
    signedDigestValue = cleanSaml "\
\<ds:SignedInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">\
    \<ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></ds:CanonicalizationMethod>\
    \<ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"></ds:SignatureMethod>\
    \<ds:Reference URI=\"#_" ++ assertionId ++ "\">\
        \<ds:Transforms>\
            \<ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"></ds:Transform>\
            \<ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></ds:Transform>\
        \</ds:Transforms>\
        \<ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"></ds:DigestMethod>\
        \<ds:DigestValue>" ++ signedDigestValue ++ "</ds:DigestValue>\
    \</ds:Reference>\
\</ds:SignedInfo>"

newSamlSignature :: String -> String -> String -> String
newSamlSignature
    assertionId
    signedDigestValue
    signedSignatureValue = cleanSaml "\
\<ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">\
    \<ds:SignedInfo>\
        \<ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\
        \<ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/>\
        \<ds:Reference URI=\"#_" ++ assertionId ++ "\">\
            \<ds:Transforms>\
                \<ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/>\
                \<ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\
            \</ds:Transforms>\
            \<ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/>\
                \<ds:DigestValue>" ++ signedDigestValue ++ "</ds:DigestValue>\
        \</ds:Reference>\
    \</ds:SignedInfo>\
    \<ds:SignatureValue>" ++ signedSignatureValue ++ "</ds:SignatureValue>\
\</ds:Signature>"

