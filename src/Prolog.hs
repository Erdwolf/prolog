{-# LANGUAGE DeriveDataTypeable, ViewPatterns, GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, IncoherentInstances, ScopedTypeVariables #-}
module Prolog
   ( Term(..), var, cut
   , Clause(..), rhs
   , VariableName(..), Atom, Unifier, Substitution, Program, Goal
   , unify, unify_with_occurs_check
   , apply
   , MonadTrace(..)
   , withTrace
   , MonadGraphGen(..)
   , runNoGraphT
   , resolve, resolve_
   , (+++)
   , consult
   , program, whitespace, comment, clause, terms, term, bottom, vname
   , main
   )
where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Monad
import Control.Arrow (first, second, (***))
import Data.Generics (Data(..), Typeable(..), everywhere, mkT, everything, mkQ)
import Data.List (intercalate)
import Data.Char (isLetter)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
--
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>),(<*>),(<$),(<*), Applicative(..))
--
import GHC.Exts (IsString(..))
--
import System.Console.Readline (readline, addHistory)


data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut Int
      deriving (Eq, Data, Typeable)
var = Var . VariableName 0
cut = Cut 0

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)
rhs (Clause   _ rhs) = const rhs
rhs (ClauseFn _ fn ) = fn

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable)

type Atom         = String
type Unifier      = [Substitution]
type Substitution = (VariableName, Term)
type Program      = [Clause]
type Goal         = Term


instance Show Term where
   show = prettyPrint False 0


prettyPrint True _ t@(Struct "," [_,_]) =
   "(" ++ prettyPrint False 0 t ++  ")"

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,InfixOp assoc name)) [l,r]) =
   parensIf (n >= p) $ prettyPrint f n_l l ++ spaced name ++ prettyPrint f n_r r
     where (n_l,n_r) = case assoc of
                           AssocLeft  -> (p-1, p)
                           AssocRight -> (p, p-1)

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,PrefixOp name)) [r]) =
   parensIf (n >= p) $ name ++ prettyPrint f (p {- Non-associative -}) r

prettyPrint _ _ t@(Struct "." [_,_]) =
   let (ts,rest) = g [] t in
      --case guard (isNil rest) >> sequence (map toChar ts) of
      --   Just str -> prettyPrint str
      --   Nothing  ->
            "[" ++ intercalate "," (map (prettyPrint True 0) ts) ++ (if isNil rest then "" else "|" ++ (prettyPrint True 0) rest) ++  "]"
   where g ts (Struct "." [h,t]) = g (h:ts) t
         g ts t = (reverse ts, t)
         isNil (Struct "[]" []) = True
         isNil _                = False

prettyPrint _ _ (Struct a [])   = a
prettyPrint _ _ (Struct a ts)   = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v)         = show v
prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ ((==cut)->True) = "!"
prettyPrint _ _ (Cut n)         = "!^" ++ show n


spaced s = let h = head s
               l = last s
           in spaceIf (isLetter h) ++ s ++ spaceIf (isLetter l || ',' == l)

spaceIf True  = " "
spaceIf False = ""

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++")"
parensIf False s = s


operatorTable :: [(String, (Int,Op))]
operatorTable = concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (name,(p,op))
       g p op@(PrefixOp name)  = (name,(p,op))

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = show i ++ "#" ++ v

instance Show Clause where
   show (Clause   lhs [] ) = show $ show lhs
   show (Clause   lhs rhs) = show $ show lhs ++ " :- " ++ intercalate ", " (map show rhs)
   show (ClauseFn lhs _  ) = show $ show lhs ++ " :- " ++ "<Haskell function>"

unify, unify_with_occurs_check :: MonadPlus m => Term -> Term -> m Unifier

unify = fix unify'

unify_with_occurs_check =
   fix $ \self t1 t2 -> if (t1 `occursIn` t2 || t2 `occursIn` t1)
                           then fail "occurs check"
                           else unify' self t1 t2
 where
   occursIn t = everything (||) (mkQ False (==t))


unify' _ Wildcard _ = return []
unify' _ _ Wildcard = return []
unify' _ (Var v) t  = return [(v,t)]
unify' _ t (Var v)  = return [(v,t)]
unify' self (Struct a1 ts1) (Struct a2 ts2) | a1 == a2 && same length ts1 ts2 =
    unifyList self (zip ts1 ts2)
unify' _ _ _ = mzero

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

unifyList :: Monad m => (Term -> Term -> m Unifier) -> [(Term, Term)] -> m Unifier
unifyList _ [] = return []
unifyList unify ((x,y):xys) = do
   u  <- unify x y
   u' <- unifyList unify (map (both (apply u)) xys)
   return (u++u')

both f (x,y) = (f x, f y)


apply :: Unifier -> Term -> Term
apply = flip $ foldl $ flip substitute
  where
    substitute (v,t) (Var v') | v == v' = t
    substitute s     (Struct a ts)      = Struct a (map (substitute s) ts)
    substitute _     t                  = t


builtins :: [Clause]
builtins =
   [ Clause (Struct "="   [var "X", var "X"]) []
   , Clause (Struct "\\=" [var "X", var "X"]) [cut, Struct "false" []]
   , Clause (Struct "\\=" [var "X", var "Y"]) []
   , Clause (Struct "not" [var "A"]) [var "A", cut, Struct "false" []]
   , Clause (Struct "not" [var "A"]) []
   , Clause (Struct "\\+" [var "A"]) [var "A", cut, Struct "false" []]
   , Clause (Struct "\\+" [var "A"]) []
   , Clause (Struct "true" []) []
   , Clause (Struct "," [var "A", var "B"]) [var "A", var "B"]
   , Clause (Struct ";" [var "A", Wildcard]) [var "A"]
   , Clause (Struct ";" [Wildcard, var "B"]) [var "B"]
   , ClauseFn (Struct "is"  [var "L", var "R"]) is
   , ClauseFn (Struct "<"   [var "N", var "M"]) (binaryIntegerPredicate (<))
   , ClauseFn (Struct ">"   [var "N", var "M"]) (binaryIntegerPredicate (>))
   , ClauseFn (Struct "=<"  [var "N", var "M"]) (binaryIntegerPredicate (<=))
   , ClauseFn (Struct ">="  [var "N", var "M"]) (binaryIntegerPredicate (>=))
   , ClauseFn (Struct "=:=" [var "N", var "M"]) (binaryIntegerPredicate (==))
   , Clause (Struct "member" [var "X", Struct "." [var "X", Wildcard]]) []
   , Clause (Struct "member" [var "X", Struct "." [Wildcard, var "Xs"]])
                [Struct "member" [var "X", var "Xs"]]
   , ClauseFn (Struct "=.." [var "Term", var "List"]) univ
   , ClauseFn (Struct "atom" [var "T"]) atom
   , ClauseFn (Struct "char_code" [var "Atom", var "Code"]) char_code
   , Clause (Struct "phrase" [var "RuleName", var "InputList"])
               [Struct "phrase" [var "RuleName", var "InputList", Struct "[]" []]]
   , Clause (Struct "phrase" [var "Rule", var "InputList", var "Rest"])
               [ Struct "=.." [var "Rule", var "L"]
               , Struct "append" [var "L", foldr cons nil (arguments [{- already in L -}] (var "InputList") (var "Rest")), var "L1"] -- FIXME This makes assumptions about "arguments"
               , Struct "=.." [var "Goal", var "L1"]
               , var "Goal"
               ]
   , Clause (Struct "append" [Struct "[]" [], var "YS", var "YS"]) []
   , Clause (Struct "append" [Struct "." [var "X", var "XS"], var "YS", Struct "." [var "X", var "XSYS"]]) [Struct "append" [var "XS", var "YS", var "XSYS"]]
   ]
 where
   binaryIntegerPredicate :: (Integer -> Integer -> Bool) -> ([Term] -> [Goal])
   binaryIntegerPredicate p [eval->Just n, eval->Just m] | n `p` m = []
   binaryIntegerPredicate p _ = [Struct "false" []]

   is [t, eval->Just n] = [Struct "=" [t, Struct (show n) []]]
   is _                 = [Struct "false" []]

   eval (Struct (reads->[(n,"")]) []) = return n :: Maybe Integer
   eval (Struct "+" [t1, t2])   = (+) <$> eval t1 <*> eval t2
   eval (Struct "*" [t1, t2])   = (*) <$> eval t1 <*> eval t2
   eval (Struct "-" [t1, t2])   = (-) <$> eval t1 <*> eval t2
   eval (Struct "mod" [t1, t2]) = mod <$> eval t1 <*> eval t2
   eval (Struct "-" [t])        = negate <$> eval t
   eval _                       = mzero

   univ [Struct a ts, list]                        = [Struct "=" [Struct "." [Struct a [], foldr cons nil ts], list]]
   univ [term,        Struct "." [Struct a [], t]] = [Struct "=" [term, Struct a (foldr_pl (:) [] t)]]
   univ _                                          = [Struct "false" []]

   atom [Struct _ []] = []
   atom _             = [Struct "false" []]

   char_code [Struct [c] [], t]               = [Struct "=" [Struct (show (fromEnum c)) [], t]]
   char_code [t, Struct (reads->[(n,"")]) []] = [Struct "=" [t, Struct [toEnum n] []]]
   char_code _                                = [Struct "false" []]


class Monad m => MonadTrace m where
   trace :: String -> m ()
instance MonadTrace (Trace IO) where
   trace = Trace . putStrLn
instance MonadTrace IO where
   trace _ = return ()
instance MonadTrace (Either err) where
   trace _ = return ()
instance (MonadTrace m, MonadTrans t, Monad (t m)) => MonadTrace (t m) where
   trace x = lift (trace x)


newtype Trace m a = Trace { withTrace :: m a }  deriving (Functor, Monad, MonadError e)

trace_ label x = trace (label++":\t"++show x)


class Monad m => MonadGraphGen m where
   createConnections :: Unifier -> [Goal] -> [Branch] -> m ()
   markSolution :: Unifier -> m ()
   markCutBranches :: Stack -> m ()

instance MonadGraphGen m => MonadGraphGen (ReaderT r m) where
   createConnections x y z = lift (createConnections x y z)
   markSolution = lift . markSolution
   markCutBranches = lift . markCutBranches


newtype NoGraphT m a = NoGraphT {runNoGraphT :: m a} deriving (Monad, Functor, MonadFix, MonadPlus, Applicative, MonadError e)
instance MonadTrans NoGraphT where
   lift = NoGraphT

instance Monad m => MonadGraphGen (NoGraphT m) where
   createConnections _ _ _ = NoGraphT $ return ()
   markSolution      _     = NoGraphT $ return ()
   markCutBranches   _     = NoGraphT $ return ()


type Stack = [(Unifier, [Goal], [Branch])]
type Branch = (Unifier, [Goal])

resolve :: (Functor m, MonadTrace m, Error e, MonadError e m) => Program -> [Goal] -> m [Unifier]
resolve program goals = runNoGraphT (resolve_ program goals)

resolve_ :: (Functor m, MonadTrace m, Error e, MonadError e m, MonadGraphGen m) => Program -> [Goal] -> m [Unifier]
-- Yield all unifiers that resolve <goal> using the clauses from <program>.
resolve_ program goals = map cleanup <$> runReaderT (resolve' 1 [] goals []) (createDB (builtins ++ program) ["false","fail"])   -- NOTE Is it a good idea to "hardcode" the builtins like this?
  where
      cleanup = filter ((\(VariableName i _) -> i == 0) . fst)

      whenPredicateIsUnknown sig action = asks (`hasPredicate` sig) >>= flip unless action

      --resolve' :: Int -> Unifier -> [Goal] -> Stack -> m [Unifier]
      resolve' depth usf [] stack = do
         trace "=== yield solution ==="
         trace_ "Depth" depth
         trace_ "Unif." usf

         markSolution usf

         (cleanup usf:) <$> backtrack depth stack
      resolve' depth usf (Cut n:gs) stack = do
         trace "=== resolve' (Cut) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   (Cut n:gs)
         mapM_ (trace_ "Stack") stack

         createConnections usf (Cut n:gs) [(usf, gs)]

         markCutBranches (take n stack)

         resolve' depth usf gs (drop n stack)
      resolve' depth usf goals@(Struct "asserta" [fact]:gs) stack = do
         trace "=== resolve' (asserta/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals
         mapM_ (trace_ "Stack") stack

         createConnections usf goals [(usf, gs)]

         local (asserta fact) $ resolve' depth usf gs stack
      resolve' depth usf goals@(Struct "assertz" [fact]:gs) stack = do
         trace "=== resolve' (assertz/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals
         mapM_ (trace_ "Stack") stack

         createConnections usf goals [(usf, gs)]

         local (assertz fact) $ resolve' depth usf gs stack
      resolve' depth usf goals@(Struct "retract" [t]:gs) stack = do
         trace "=== resolve' (retract/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals
         mapM_ (trace_ "Stack") stack

         createConnections usf goals [(usf, gs)]

         clauses <- asks getClauses
         case [ t' | Clause t' [] <- clauses, isJust (unify t t') ] of
            []       -> return (fail "retract/1")
            (fact:_) -> local (abolish fact) $ resolve' depth usf gs stack
      resolve' depth usf (nextGoal:gs) stack = do
         trace "=== resolve' ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   (nextGoal:gs)
         mapM_ (trace_ "Stack") stack
         let sig = signature nextGoal
         whenPredicateIsUnknown sig $ do
            throwError $ strMsg $ "Unknown predicate: " ++ show sig
         branches <- getBranches

         createConnections usf (nextGoal:gs) branches

         choose depth usf gs branches stack
       where
         getBranches = do
            clauses <- asks getClauses
            return $ do
               clause <- renameVars clauses
               u <- unify (apply usf nextGoal) (lhs clause)
               let newGoals = rhs clause (map snd u)
               let u' = usf +++ u
               let gs'  = map (apply u') $ newGoals ++ gs
               let gs'' = everywhere (mkT shiftCut) gs'
               return (u', gs'')

         shiftCut (Cut n) = Cut (succ n)
         shiftCut t       = t

         renameVars = everywhere $ mkT $ \(VariableName _ v) -> VariableName depth v

      choose depth _ _  []              stack = backtrack depth stack
      choose depth u gs ((u',gs'):alts) stack = do
         trace "=== choose ==="
         trace_ "Depth"   depth
         trace_ "Unif."   u
         trace_ "Goals"   gs
         mapM_ (trace_ "Alt.") ((u',gs'):alts)
         mapM_ (trace_ "Stack") stack
         resolve' (succ depth) u' gs' ((u,gs,alts) : stack)

      backtrack _     [] = do
         trace "=== give up ==="
         return (fail "Goal cannot be resolved!")
      backtrack depth ((u,gs,alts):stack) = do
         trace "=== backtrack ==="
         choose (pred depth) u gs alts stack

u1 +++ u2 = simplify $ u1 ++ u2

simplify :: Unifier -> Unifier
simplify u = map (second (apply u)) u



newtype Signature = Signature (Atom, Int) deriving (Ord, Eq)
instance Show Signature where
   show (Signature (name, arity)) = name ++ "/" ++ show arity

signature :: Term -> Signature
signature (Struct name ts) = Signature (name, length ts)


data Database = DB [Clause] (Set Signature)

hasPredicate (DB _ signatures) sig = sig `Set.member` signatures

createDB clauses emptyPredicates = DB clauses (Set.fromList $ map (signature . lhs) clauses ++ [ Signature (name,0) | name <- emptyPredicates ])

getClauses (DB clauses _) = clauses

asserta fact (DB clauses signatures) = DB ([Clause fact []] ++ clauses) (Set.insert (signature fact) signatures)
assertz fact (DB clauses signatures) = DB (clauses ++ [Clause fact []]) (Set.insert (signature fact) signatures)
abolish fact (DB clauses signatures) = DB (deleteFact clauses) (signatures {- Once known, the signature is never removed. -} )
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []


{- Parser -}
consult = fmap consultString . readFile

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

program = many (clause <* char '.' <* whitespace)

whitespace = skipMany (comment <|> skip space <?> "")
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]

skip = (>> return ())

clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      normal t = do
            ts <- option [] $ do string ":-" <* whitespace
                                 terms
            return (Clause t ts)

      dcg t = do
            string "-->" <* whitespace
            ts <- terms
            return (translate (t,ts))

      translate ((Struct a ts), rhs) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' rhs'

      translate' t s s0 | isList t   = Struct "=" [ s, foldr_pl cons s0 t ]     -- Terminal
      translate' t@(Struct "{}" ts) s s0 = foldr and (Struct "=" [ s, s0 ]) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)             -- Non-Terminal

      and x y = Struct "," [x,y]

arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts

--infix 6 \\
--x \\ y = Struct "\\" [x,y]

isList (Struct "." [_,_]) = True
isList (Struct "[]" [])   = True
isList _                  = False

foldr_pl f k (Struct "." [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct "[]" [])   = k


terms = sepBy1 termWithoutConjunction (charWs ',')

term = term' False
termWithoutConjunction = term' True

data Op = PrefixOp String
        | InfixOp Assoc String

hierarchy :: Bool -> [[Op]]
hierarchy ignoreConjunction =
   --[ [ InfixOp NonAssoc "-->", InfixOp NonAssoc ":-" ]
   [ [ infixR ";" ] ] ++
   (if ignoreConjunction then [] else [ [ infixR "," ] ])  ++
   [ [ prefix "\\+" ]
   , map infixL ["<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
   , map infixL ["+", "-", "\\"]
   , [ infixL "*"]
   , [ infixL "mod" ]
   , [ prefix "-" ]
   , [ prefix "$" ] -- used for quasi quotation
   ]
 where
   prefix = PrefixOp
   infixL = InfixOp AssocLeft
   infixR = InfixOp AssocRight


term' ignoreConjunction = buildExpressionParser (reverse $ map (map toParser) $ hierarchy ignoreConjunction) (bottom <* whitespace)

bottom = variable
      <|> struct
      <|> list
      <|> stringLiteral
      <|> cut <$ char '!'
      <|> Struct "{}" <$> between (charWs '{') (char '}') terms
      <|> between (charWs '(') (char ')') term

toParser (PrefixOp name) = Prefix (do{ reservedOp name; return (\t -> Struct name [t]) })
toParser (InfixOp assoc name) = Infix (do{ reservedOp name; return (\t1 t2 -> Struct name [t1, t2]) }) assoc
reservedOp = P.reservedOp $ P.makeTokenParser $ emptyDef
   { P.opStart = oneOf ";,<=>\\i*+m@"
   , P.opLetter = oneOf "=.:<+"
   , P.reservedOpNames = operatorNames
   , P.caseSensitive = True
   }

charWs c = char c <* whitespace

operatorNames = [ ";", ",", "<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "*", "+", "-", "\\", "mod", "\\+" ]

variable = (Wildcard <$ try (char '_' <* notFollowedBy (alphaNum <|> char '_')))
       <|> Var <$> vname
       <?> "variable"

vname = VariableName 0 <$> ((:) <$> upper    <*> many  (alphaNum <|> char '_') <|>
                            (:) <$> char '_' <*> many1 (alphaNum <|> char '_'))

atom = (:) <$> lower <*> many (alphaNum <|> char '_')
   <|> many1 digit
   <|> choice (map string operatorNames)
   <|> many1 (oneOf "#$&*+/.<=>\\^~")
   <|> between (char '\'') (char '\'') (many (noneOf "'"))
   <?> "atom"

struct = do a <- atom
            ts <- option [] $ between (charWs '(') (char ')') terms
            return (Struct a ts)

list = between (charWs '[') (char ']') $
         flip (foldr cons) <$> option []  terms
                           <*> option nil (charWs '|' >> term)

cons t1 t2 = Struct "."  [t1,t2]
nil        = Struct "[]" []

stringLiteral = foldr cons nil . map representChar <$> between (char '"') (char '"') (try (many (noneOf "\"")))

representChar c = Struct (show (fromEnum c)) [] -- This is the classical Prolog representation of chars as code points.
--representChar c = Struct [c] [] -- This is the more natural representation as one-character atoms.
--representChar c = Struct "char" [Struct (show (fromEnum c)) []] -- This is a representation as tagged code points.
--toChar :: Term -> Maybe Char
--toChar (Struct "char" [Struct (toEnum . read->c) []]) = Just c
--toChar _                                              = Nothing


{- Allow specification through string literals by using OverloadedStrings -}
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


-- SWI-Prolog-style interactive prompt:
main = do putStrLn "Prolog.hs interactive prompt. (Exit with Ctrl-D)"
          forever $ readline "?- " >>= handleExit >>= \input -> case parse (terms <* char '.' <* eof) "(input)" input of Left e -> print e; Right ts -> addHistory input >> either putStrLn printSolutions (resolve [] ts)
handleExit = maybe (fail "End of input") return
printSolutions [] = putStrLn "false.\n"
printSolutions (x:xs) = putStr (if null x then "true" else intercalate ",\n" [ show k ++ " = " ++ show v | (k,v) <- x ]) >> getChar >>= \c -> case c of ' ' -> putStrLn ";" >> printSolutions xs; _ -> putStrLn ""
