-- SWI-Prolog-style interactive prompt.
--
module Console (main) where

import Control.Applicative ((<*))
import Control.Monad (forever)
import Data.List (intercalate)
import System.IO

import System.Console.Readline (readline, addHistory)
import Text.Parsec (parse, char, eof)

import Language.Prolog (terms, resolve)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          putStrLn "Prolog.hs interactive prompt. (Exit with Ctrl-D)"
          forever $ do
            input <- readline "?- " >>= handleExit
            case parse (terms <* char '.' <* eof) "(input)" input of
               Left e -> do
                  print e
               Right ts -> do
                  addHistory input
                  either putStrLn printSolutions (resolve [] ts)

handleExit = maybe (fail "End of input") return

printSolutions [] = do
   putStrLn "false.\n"
printSolutions (u:us) = do
   if null u
      then putStr "true"
      else putStr $ intercalate ",\n" [ show k ++ " = " ++ show v | (k,v) <- u ]
   c <- getChar
   case c of
      ' ' -> do putStrLn ";"
                printSolutions us;
      _   -> do putStrLn ""
