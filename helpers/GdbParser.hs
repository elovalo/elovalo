{-# LANGUAGE RecordWildCards #-}
module GdbParser (RawDump(..),gdbOutput) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data RawDump = RawDump { start  :: Integer   -- ^Word16 count from start
                       , values :: [Integer]
                       } deriving (Show)

tp = makeTokenParser javaStyle

gdbOutput :: Parser [[RawDump]]
gdbOutput = do
  headGarbage
  many frame

headGarbage = many $ do
  notFollowedBy $ try frameStart
  skipLine

frame = do
  frameStart
  newline
  xs <- many $ do
    -- Not the end of the frame
    try $ notFollowedBy frameStart
    -- Either correct dump line or some garbage output
    try (Just <$> memdump) <|> (skipLine >> return Nothing)
  -- Strip garbage lines
  return $ catMaybes xs

-- |Parses BREAK instruction
frameStart :: Parser String
frameStart = string "Program received signal SIGTRAP, Trace/breakpoint trap."

-- |Parses one dump
memdump :: Parser RawDump
memdump = do
  many1 digit
  string ": /x *(gs_buf_front+"
  startRaw <- natural tp
  string ")@"
  count <- natural tp
  symbol tp "="
  values <- braces tp $ commaSep tp $ char '0' >> hexadecimal tp
  let (start,mod) = startRaw `divMod` 2
  unless (mod == 0) $ fail "Dump address must be multiple of 2"
  unless (count == toInteger (length values)) $
    fail "Data dump length mismatches"
  return RawDump{..}

skipLine = manyTill anyChar newline
