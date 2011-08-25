{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Quote (t,ts,c,pl) where

import Control.Applicative ((<*))
import Data.Functor.Identity (Identity)

import Language.Haskell.TH (listE, varE, viewP, mkName, Q, Exp, Pat)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parsec (parse, eof, ParsecT)
import Data.Generics (extQ, typeOf, Data)

import Prolog ( Term(..), VariableName, Clause(..), Goal
              , term, terms, clause, program, whitespace
              )

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
      Right x -> const (fail $ "Quasi-quoted expressions of type " ++ show (typeOf x) ++ " are not implemented.")
          `extQ` unquote                     -- Term
          `extQ` (listE . map unquote)       -- [Term]
          `extQ` unquoteClause               -- Clause
          `extQ` (listE . map unquoteClause) -- [Clause]
           $ x
      Left e -> fail (show e)
  where
   unquote (Struct "$" [Struct var []]) =
                             [e| Struct (show $(varE (mkName var))) [] |]
   unquote (Struct "$" _)  = fail "Found '$' with non-unquotable arguments"
   unquote (Struct a   ts) = [e| Struct a $(listE $ map unquote ts) |]
   unquote t               = lift t

   unquoteClause (Clause lhs rhs) =
      [e| Clause $(unquote lhs) $(listE $ map unquote rhs) |]
   unquoteClause (ClauseFn _ _) =
      fail "Clauses using Haskell functions are not quasi-quotable."


parsePrologPat :: (Data a, Lift a) => ParsecT [Char] () Identity a -> String -> String -> Q Pat
parsePrologPat parser name str = do
   case parse (whitespace >> parser <* eof) ("(Prolog " ++ name ++ " pattern)") str of
      Right x -> viewP [e| (== $(lift x)) |] [p| True |]
