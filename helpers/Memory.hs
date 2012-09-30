{-# LANGUAGE RecordWildCards #-}
module Memory where

import Data.Array
import GdbParser (RawDump(..))
import Parameters

-- |Divides integral number and returns the ceiling of the result
divCeil :: Integral a => a -> a -> a
divCeil a b = (a+b-1) `div` b

frameArrayLength = (ledsX*ledsY*ledsZ*voxelBits) `divCeil` 16

dumpToAssoc RawDump{..} = zip [start..] values

frameToArray :: [RawDump] -> Array Integer Integer
frameToArray frame = foldl arrayUpdate empty frame
  where
    empty = listArray (0,frameArrayLength-1) missingList
    missingList = map missing [0..]
    missing i = error $ "Frame dump is missing value for "++show i
    arrayUpdate a x = a//dumpToAssoc x

