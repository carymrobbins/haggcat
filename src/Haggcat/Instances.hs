module Haggcat.Instances where

import Control.Monad (liftM2)
import Control.Monad.Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Haggcat.Classes

instance MonadZip Maybe where
    mzip = liftM2 (,)

instance Addable BS.ByteString where
    (<+>) = BS.append

instance Addable LBS.ByteString where
    (<+>) = LBS.append

