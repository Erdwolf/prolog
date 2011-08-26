module Specs (main) where

import Control.Applicative ((<*),(<*>),(<$>))
import Control.Monad (forM)
import System.Directory
import System.FilePath
import System.Console.ANSI
import System (getArgs)

import Test.HUnit
import Text.Parsec

import Prolog (vname, term, bottom, resolve, consult)


main = do
  args <- getArgs
  let specRoot = case args of [] -> "."; [path] -> path
  files <- filter ((".spec"==) . takeExtension) <$> map (specRoot </>) <$> getDirectoryContents specRoot
  tests <- forM files $ \fname -> do
    let fixture = replaceExtension fname ".pl"
    hasFixture <- doesFileExist fixture
    p <- if hasFixture
            then do Right p <- consult fixture
                    return p
            else return []
    text <- readFile fname
    case parse (specFile p) fname text of
      Left err -> return (dropExtension fname ~: assertFailure (show err))
      Right tests -> return (dropExtension fname ~: tests)
  colorizeResult =<< runTestTT (test tests)

colorizeResult result = do
  let color = case result of Counts _ _ 0 0 -> Green
                             _              -> Red
  cursorUpLine 1
  setSGR [SetColor Background Dull color, SetColor Foreground Dull Black]
  putStr (showCounts result)
  setSGR [Reset]
  putStrLn ""


specFile p = testSpec p `sepBy` newline <* eof

testSpec p = do
  q  <- string "?- " >> term <* string "." <* newline
  us <- unifiers <* optional (string " ;" >> newline >> string "false") <* string "." <* ((newline >> return ()) <|> eof)
  return $ "?- " ++ show q ++ "." ~: resolve p [q] >>= (@?= us)

unifiers =  (unifier `sepBy1` (try $ string " ;" <* newline >> notFollowedBy (string "false.")))
        <|> (string "false" >> return [])

unifier =  (substitution `sepBy1` (string "," <* newline))
       <|> (string "true" >> return [])

substitution = (,) <$> vname <* string " = " <*> bottom
