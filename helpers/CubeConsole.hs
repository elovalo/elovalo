{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- |Somewhat unrealible code for LED cube clock. Serial communication
-- hangs randomly.
module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Attoparsec.Binary
import Data.Binary (encode)
import Data.Char (isSpace)
import Data.Hex
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process (rawSystem)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL

main = do
  -- TODO Migrate to cmdargs later
  [cmd,dev] <- getArgs
  
  configureSerialPort dev
  h <- openBinaryFile dev ReadWriteMode
  BS.hGetNonBlocking h 256 -- Probably empty the buffer 
 
  case cmd of
    "-g" -> do
      BS.hPut h "\x7e\x05"
      time <- A.parseWith (BS.hGetSome h 256) parseSecs BS.empty
      print time
    "-s" -> setTimeCmd >>= BL.hPutStr h
      
  hClose h

-- |Configures serial port for Active Robots I2C converter.
configureSerialPort :: FilePath -> IO ()
configureSerialPort devPath = do
  code <- rawSystem "stty" ["-F",devPath,"9600","cs8","cstopb","-parenb","raw"]
  unless (code==ExitSuccess) $ fail "Serial port configuration failed."

setTimeCmd = do
  now <- getPOSIXSecs
  let coded = encode (now :: Word32)
  return $ BL.concat [BL.pack "\x7E\x04",escapeBS coded]

getPOSIXSecs :: (Integral a) => IO a
getPOSIXSecs = round <$> getPOSIXTime

parseSecs :: A.Parser UTCTime
parseSecs = do
  A.word8 0x7e
  A.word8 0x06
  secs <- anyWord32be
  return $ posixSecondsToUTCTime $ realToFrac secs

escapeBS = BL.concatMap escaper
  where escaper '\x7e' = BL.pack "\x7e\x00"
        escaper x = BL.singleton x
