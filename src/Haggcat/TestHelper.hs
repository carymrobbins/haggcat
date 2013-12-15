module Haggcat.TestHelper where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import System.FilePath

import Haggcat.Client

getTestClient :: FilePath -> IO Client
getTestClient dir = liftM5 Client
    (readTestData dir "consumerKey")
    (readTestData dir "consumerSecret")
    (readTestData dir "issuerId")
    (readTestData dir "customerId")
    (return $ dir </> "certificate.key")

readTestData :: FilePath -> String -> IO C.ByteString
readTestData dir = liftM (C.pack . strip) . readFile . (dir </>)

