module Common where

import Data.Word

data RawDump = RawDump { start  :: Int   -- ^Word16 count from start
                       , values :: [Word16]
                       } deriving (Show)
