{-# LANGUAGE DeriveDataTypeable #-}
module ParseArgs (
   parseArgs
) where

import System.Console.CmdArgs
import System.Environment (getProgName)

data Options = Options
   { query :: String
   , file  :: [String]
   , output :: String
   , positional :: [String]
   }
  deriving (Data, Typeable)

options = Options
  { query  = def &= typ "QUERY" &= help "Set Prolog query (If not set, first positional argument is used)"
  , file   = def &= typ "FILE"  &= help "Consult file before executing query"
  , output = "graph.png" &= typ "FILE"  &= help "Save generated image to file (default: 'graph.png')"
  , positional = def &= args &= typ "QUERY [FILE]..."
  }
  &= versionArg [ignore]
  &= helpArg [name "h"]

parseArgs = do
   opts <- getProgName >>= cmdArgs . ((options &=) . program)
   return $ case opts of
      Options q fs o []      -> (q, fs,      o)
      Options _ fs o [q]     -> (q, fs,      o)
      Options _ fs o (q:fs') -> (q, fs++fs', o)
