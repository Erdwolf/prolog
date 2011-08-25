-- Allow specification through string literals by using OverloadedStrings.
--
module IsString () where

import GHC.Exts (IsString(..))
import Control.Applicative ((<*))
import Text.Parsec (parse, eof)

import Prolog ( vname,        term, clause
              , VariableName, Term, Clause )


instance IsString Clause where
   fromString s =
      case parse (clause <* eof) "(Clause literal)" s of
         Left  e -> error (show e)
         Right c -> c
instance IsString Term where
   fromString s =
      case parse (term <* eof) "(Term literal)" s of
         Left  e -> error (show e)
         Right c -> c
instance IsString VariableName where
   fromString s =
      case parse (vname <* eof) "(VariableName literal)" s of
         Left  e -> error (show e)
         Right c -> c
