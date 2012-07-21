-- This module helps to find changes in the produced JSON.
module ExamineEffect where

import Text.JSON

-- |Reads JSON from given path and extract frames from it.
getFrames :: FilePath -> IO [[Float]]
getFrames f = do
  json <- readFile f
  case (decode json >>= valFromObj "frames" ) of
    Ok x    -> return x
    Error e -> fail e

-- |Gets difference between pixel values.
diff a b = zipWith (-) (concat b) (concat a)

-- |Get maximum and minimum difference between two frame lists.
maxChanges a b = (minimum list, maximum list)
  where list = diff a b
