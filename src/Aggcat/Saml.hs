module Aggcat.Saml where

import qualified Codec.Crypto.RSA as RSA
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

foo :: String -> LBS.ByteString
foo = LC.pack

isoFormatTime :: UTCTime -> String
isoFormatTime = formatTime defaultTimeLocale "%FT%T%QZ"

newIsoTime :: IO String
newIsoTime = liftM isoFormatTime getCurrentTime

uuidToHex :: U.UUID -> String
uuidToHex uuid = concat . map ((flip showHex) "") $ [a,b,c,d]
  where
    (a,b,c,d) = U.toWords uuid

newHexUUID :: IO String
newHexUUID = liftM uuidToHex nextRandom

cleanSaml :: String -> String
cleanSaml = strip . (flip (subRegex (mkRegex ">\\s*<"))) "><"

data Saml = Saml
    { assertionId :: String
    , issueInstant :: UTCTime
    , notBefore :: UTCTime
    , notOnOrAfter :: UTCTime
    , consumerKey :: String
    , customerId :: String
    , signature :: String
    } deriving (Show)

newSaml :: String -> String -> IO Saml
newSaml consumerKey customerId = do
    assertionId <- newHexUUID
    now <- getCurrentTime
    return $ Saml
        { assertionId=assertionId
        , consumerKey=consumerKey
        , issueInstant=now
        , notBefore=addUTCTime (-5 * 60) now
        , notOnOrAfter=addUTCTime (10 * 60) now
        , customerId=customerId
        , signature=""
        }

--signedSignatureValue :: Saml -> RSA.PrivateKey -> LBS.ByteString
signedSignatureValue saml privateKey =
    Base64.encode . RSA.sign privateKey . SHA1.hashlazy . LC.pack $ samlSignedInfo
        (assertionId saml)
        (C.unpack . signedDigestValue $ saml)

--signedDigestValue :: Saml -> LBS.ByteString
signedDigestValue saml =
    Base64.encode . SHA1.hashlazy . LC.pack $ assertion
  where
    assertion = samlAssertion
        (assertionId saml)
        (isoFormatTime . issueInstant $ saml)
        (isoFormatTime . notBefore $ saml)
        (isoFormatTime . notOnOrAfter $ saml)
        (consumerKey saml)
        (signature saml)
        (customerId saml)

samlAssertion :: String -> String -> String -> String -> String -> String -> String -> String 
samlAssertion
    assertionId
    isoNow
    isoNotBefore
    isoNotAfter
    samlIdentityProviderId
    signature
    customerId = cleanSaml "\
\  <saml2:Issuer>" ++ samlIdentityProviderId ++ "</saml2:Issuer>" ++ signature ++ "<saml2:Subject>\
\    <saml2:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\">" ++ customerId ++ "</saml2:NameID>\
\    <saml2:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"></saml2:SubjectConfirmation>\
\  </saml2:Subject>\
\  <saml2:Conditions NotBefore=\"" ++ isoNotBefore ++ "\" NotOnOrAfter=\"" ++ isoNotAfter ++ "\">\
\    <saml2:AudienceRestriction>\
\      <saml2:Audience>" ++ samlIdentityProviderId ++ "</saml2:Audience>\
\    </saml2:AudienceRestriction>\
\  </saml2:Conditions>\
\  <saml2:AuthnStatement AuthnInstant=\"" ++ isoNow ++ "\" SessionIndex=\"_" ++ assertionId ++ "\">\
\    <saml2:AuthnContext>\
\      <saml2:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:unspecified</saml2:AuthnContextClassRef>\
\    </saml2:AuthnContext>\
\  </saml2:AuthnStatement>\
\</saml2:Assertion>"

samlSignedInfo :: String -> String -> String
samlSignedInfo
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

samlSignature :: String -> String -> String -> String
samlSignature
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

