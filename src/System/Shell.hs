module System.Shell
  ( Var (..),
    Arith (..),
    Term,
    str,
    arith,
    var,
    output,
    quote,
    quoteTerms,
    Assign,
    (@=),
    Test (..),
    Shell (..),
    (|>),
    (|>>),
    (<|),
    (-|-),
    aoTerm,
    notC,
    andC,
    andCS,
    orC,
    orCS,
    localVar,
    newEnv,
    forCmdVar,
  )
where

import System.Shell.Internal.Shell

str :: t -> Term t m
str = StrTerm

arith :: Arith t -> Term t m
arith = ArithTerm

(@=) :: Var t -> Term t m2 -> Assign t m2
(Var a) @= b = Assign a b

(|>) :: (Shell t m) => m () -> Term t m -> m ()
c |> t = redir c [Redir Out t]

(|>>) :: (Shell t m) => m () -> Term t m -> m ()
c |>> t = redir c [Redir Append t]

(<|) :: (Shell t m) => m () -> Term t m -> m ()
c <| t = redir c [Redir In t]

(-|-) :: Shell t m => m () -> m () -> m ()
l -|- r = pipe [l, r]

aoTerm :: m () -> AndOrTerm m
aoTerm = AoCmd

notC :: m () -> AndOrTerm m
notC = AoNot

andC :: AndOrTerm m -> AndOrTerm m -> AndOrCmd m
andC l r = AndCmd l (AndOrTerm r)

andCS :: AndOrTerm m -> AndOrCmd m -> AndOrCmd m
andCS = AndCmd

orC :: AndOrTerm m -> AndOrTerm m -> AndOrCmd m
orC l r = OrCmd l (AndOrTerm r)

orCS :: AndOrTerm m -> AndOrCmd m -> AndOrCmd m
orCS = OrCmd

localVar :: Shell t m => Assign t m -> m (Var t)
localVar t@(Assign n _) = localVars [t] >> return (Var n)

newEnv :: Shell t m => Assign t m -> m (Var t)
newEnv t@(Assign n _) = newEnvs [t] >> return (Var n)

forCmdVar :: Shell t m => t -> [Term t m] -> (Term t m -> m ()) -> m ()
forCmdVar t ts b =
  localVar (Var t @= "") >>= \vt ->
    forCmd t ts (b (var vt))