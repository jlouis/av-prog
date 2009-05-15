module Ocult.Ast(
                Term(..),
                Pattern(..),
                Rule(..),
                docPattern
) where

import Data.List
import Text.PrettyPrint

-- Terms are the objects the language manipulates
data Term a = TConst a | TApp (Term a) (Term a)

-- Patterns are things we *MATCH* terms against and substitute by. A program defines these
data Pattern a b = PConst a | PVar b | PApp (Pattern a b) (Pattern a b) | PSingleton String

-- A Rule defines a pattern to *MATCH* and a *REPLACEMENT* pattern.
data Rule a b = Rl (Pattern a b) (Pattern a b)

-- A Program (sentence of advise) is a list of Rules
type Program a b = [Rule a b]

-- Contexts define what the current variable bindings are
type Context a b = [(b, Term a)]

-- Pretty-printing of Ocult
docTerm :: Show a => Term a -> Doc
docTerm tm =
    case tm of
      TConst c -> text "Const " <> (text $ show c)
      TApp t1 t2 -> parens $ t1' <> space <> t2'
           where
             t1' = docTerm t1
             t2' = docTerm t2

instance Show a => Show (Term a) where
    show = render . docTerm

docPattern :: (Show a, Show b) => Pattern a b -> Doc
docPattern pat =
    case pat of
      PConst c -> text "Const " <> (text $ show c)
      PVar v   -> text "Var " <> (text $ show v)
      PApp p1 p2 -> parens $ p1' <> space <> p2'
          where
            p1' = docPattern p1
            p2' = docPattern p2

instance (Show a, Show b) => Show (Pattern a b) where
    show = render . docPattern

docRule :: (Show a, Show b) => Rule a b -> Doc
docRule (Rl pattern replacer) =
    parens (pattern' <> text " => " <> replacer') <> semi
        where
          pattern' = docPattern pattern
          replacer' = docPattern replacer

instance (Show a, Show b) => Show (Rule a b) where
    show = render . docRule

docProgram :: (Show a, Show b) => Program a b -> Doc
docProgram prg =
    hcat $ map docRule prg

instance (Show a, Show b) => Show (Program a b) where
    show = render . docProgram

-- Look up the term in a context
findContext :: Eq b => Context a b -> b -> Maybe (Term a)
findContext context var =
    find (\(v, tm) -> v == var) context >>= (return . snd)

-- Substitute a patterns variables with what is in a context.
patternSubst :: Eq b => Context a b -> Pattern a b -> Term a
patternSubst context pattern = pSubst pattern
  where
    pSubst (PConst k) = (TConst k)
    pSubst (PVar k)   = case findContext context k of
                          Nothing -> error "Impossible"
                          Just tm -> tm
    pSubst (PApp p1 p2) = TApp (pSubst p1) (pSubst p2)







