{-# LANGUAGE RecordWildCards #-}
-- |Memory conversion functions. Takes in memory dump in RawDump lists
-- and produces voxel intensity lists.
module Memory (dumpToNormalized) where

import Control.Monad (replicateM)
import Data.Array
import Data.Binary.Put
import Data.Binary.Get (runGet)
import Data.Binary.Bits.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Common
import Parameters

-- |Divides integral number and returns the ceiling of the result
divCeil :: Integral a => a -> a -> a
divCeil a b = (a+b-1) `div` b

frameArrayLength :: Int
frameArrayLength = (ledsX*ledsY*ledsZ*voxelBits) `divCeil` 16

dumpToAssoc RawDump{..} = zip [start..] values

frameToArray :: [RawDump] -> Array Int Word16
frameToArray frame = foldl arrayUpdate empty frame
  where
    empty = listArray (0,frameArrayLength-1) missingList
    missingList = map missing [0..]
    missing i = error $ "Frame dump is missing value for "++show i
    arrayUpdate a x = a//dumpToAssoc x

arrayToBinary :: Array Int Word16 -> ByteString
arrayToBinary a = runPut $ mapM_ putWord16le $ elems a

binaryToVoxels :: ByteString -> [Word16]
binaryToVoxels = runGet $ runBitGet getter
  where getter = replicateM (ledsX*ledsY*ledsZ) $ getWord16be voxelBits

normalize x = fromIntegral x / (2^voxelBits-1)

-- |Takes in list of frames and produces intensities.
dumpToNormalized :: [[RawDump]] -> [[Double]]
dumpToNormalized = map f
  where f = map normalize . binaryToVoxels . arrayToBinary . frameToArray
