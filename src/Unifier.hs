module Unifier
   ( Unifier, Substitution
   , unify, unify_with_occurs_check
   , apply, (+++)
   )
where

import Control.Monad (MonadPlus, mzero)
import Control.Arrow (second)
import Data.Function (fix)
import Data.Generics (everything, mkQ)

import Syntax

type Unifier      = [Substitution]
type Substitution = (VariableName, Term)


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

u1 +++ u2 = simplify $ u1 ++ u2

simplify :: Unifier -> Unifier
simplify u = map (second (apply u)) u


apply :: Unifier -> Term -> Term
apply = flip $ foldl $ flip substitute
  where
    substitute (v,t) (Var v') | v == v' = t
    substitute s     (Struct a ts)      = Struct a (map (substitute s) ts)
    substitute _     t                  = t
