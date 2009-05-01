module OcultAst(
                Term(..),
                Pattern(..)
) where

import Data.List

-- Terms are the objects the language manipulates
data Term a = TConst a | TApp (Term a) (Term a)

-- Patterns are things we *MATCH* terms against and substitute by. A program defines these
data Pattern a b = PConst a | PVar b | PApp (Pattern a b) (Pattern a b)

-- A Rule defines a pattern to *MATCH* and a *REPLACEMENT* pattern.
data Rule = Rl (Pattern Int Int) (Pattern Int Int)

-- A Program (sentence of advise) is a list of Rules
type Program = [Rule]

-- Contexts define what the current variable bindings are
type Context a = [(a, Term a)]

-- Look up the term in a context
findContext :: Eq a => Context a -> a -> Maybe (Term a)
findContext context var =
    find (\(v, tm) -> v == var) context >>=
         (\(v, tm) -> return tm)

-- Substitute a patterns variable with what is in a context.
patternSubst :: Context Int -> Pattern Int Int -> Term Int
patternSubst context pattern = pSubst pattern
  where
    pSubst (PConst k) = (TConst k)
    pSubst (PVar k)   = case findContext context k of
                          Nothing -> error "Impossible"
                          Just tm -> tm
    pSubst (PApp p1 p2) = TApp (pSubst p1) (pSubst p2)








