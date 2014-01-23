module Haggcat.Classes where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

class Addable a where
    (<+>) :: a -> a -> a

instance Addable BS.ByteString where
    (<+>) = BS.append

instance Addable LBS.ByteString where
    (<+>) = LBS.append

