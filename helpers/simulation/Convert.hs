module Convert (gdbFileToJson) where

import Text.JSON
import Text.Parsec.String
import GdbParser
import Memory
import Parameters

-- |Reads GNU debugger output and writes to JSON file
gdbFileToJson :: FilePath -> FilePath -> IO ()
gdbFileToJson input output = do
  Right x <- parseFromFile gdbOutput input
  writeFile output $ encode $ jsonify $ dumpToNormalized x

jsonify x = makeObj [("fps",showJSON fps)
                    ,("geometry",showJSON [ledsX,ledsY,ledsZ])
                    ,("frames",showJSON x)
                    ]
