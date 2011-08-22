{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Quote (t,ts,c,pl) where

import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift
import Language.Haskell.TH hiding (Clause, clause)
import Language.Haskell.TH.Quote
import Prolog
import Text.Parsec
import Control.Applicative
import Data.Functor.Identity
import Data.Generics

$(deriveLiftMany [''Term, ''VariableName, ''Clause])

instance Lift ([Term] -> [Goal]) where
   lift _ = fail "Clauses using Haskell functions can't be lifted."


t  = prologQuasiQuoter term    "term"
ts = prologQuasiQuoter terms   "term list"
c  = prologQuasiQuoter clause  "clause"
pl = prologQuasiQuoter program "program"

prologQuasiQuoter parser name =
   QuasiQuoter { quoteExp  = parsePrologExp parser name
               , quotePat  = parsePrologPat parser name
               , quoteType = fail ("Prolog "++ name ++"s can't be Haskell types!")
               , quoteDec  = fail ("Prolog "++ name ++"s can't be Haskell declarations!")
               }

parsePrologExp :: (Data a, Lift a) => ParsecT [Char] () Identity a -> String -> String -> Q Exp
parsePrologExp parser name str = do
   case parse (whitespace >> parser <* eof) ("(Prolog " ++ name ++ " expression)") str of
      Right x -> ((const $ fail "fsdsfsd") `extQ` (listE . map foo) `extQ` foo) x --dataToExpQ (const Nothing `extQ` (Just . foo)) x
  where
   foo (Struct "$" [Struct var []]) = [e| Struct (show $(varE (mkName var))) [] |]
   foo (Struct "$" _)  = fail "Found '$' with non-unquotable arguments"
   foo (Struct a   ts) = [e| Struct a $(listE $ map foo ts) |]
   foo t               = lift t


parsePrologPat :: (Data a, Lift a) => ParsecT [Char] () Identity a -> String -> String -> Q Pat
parsePrologPat parser name str = do
   case parse (whitespace >> parser <* eof) ("(Prolog " ++ name ++ " pattern)") str of
      Right x -> viewP [e| (== $(lift x)) |] [p| True |]
