module Main where

import Control.Applicative ((<$>))
import Control.Arrow ((>>>),(<<<))
import Language.Prolog (consult, parseQuery)
import ParseArgs (parseArgs)
import GraphViz (resolveTree, resolveTreeToFile)

main = do
   (queryString, files, output) <- parseArgs
   p <- concat <$> mapM ((abortOnError=<<) . consult) files
   q <- abortOnError $ parseQuery queryString
   resolveTreeToFile output p q
   --case output of
   --   Nothing -> resolveTree p q {- FIXME Never shown since thread terminated. -}
   --   Just file -> resolveTreeToFile file p q >> return ()

abortOnError = either (error . show) return
