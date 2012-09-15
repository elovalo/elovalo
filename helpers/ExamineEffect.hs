--
-- Copyright 2012 Elovalo project group 
-- 
-- This file is part of Elovalo.
-- 
-- Elovalo is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- Elovalo is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
--
--

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
