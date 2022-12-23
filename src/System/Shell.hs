module System.Shell
  ( Var (..),
    Arith (..),
    Term,
    str,
    arith,
    var,
    varLength,
    varSuffix,
    varSubstr,
    TrimMatch (..),
    varTrimPrefix,
    varTrimSuffix,
    ReplaceOpt (..),
    varReplace,
    output,
    quote,
    quoteTerms,
    Assign,
    (@=),
    Test (..),
    ShellStr (..),
    Shell (..),
    (|>),
    (|>>),
    (<|),
    (<<|),
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

varLength :: Var t -> Term t m
varLength (Var v) = VarTerm v VarLength

varSuffix :: Var t -> Term t m -> Term t m
varSuffix (Var v) o = VarTerm v (Suffix o)

varSubstr :: Var t -> Term t m -> Term t m -> Term t m
varSubstr (Var v) o l = VarTerm v (Substr o l)

varTrimPrefix :: Var t -> TrimMatch -> Term t m -> Term t m
varTrimPrefix (Var v) tm p = VarTerm v (Trim PrefixD tm p)

varTrimSuffix :: Var t -> TrimMatch -> Term t m -> Term t m
varTrimSuffix (Var v) tm p = VarTerm v (Trim SuffixD tm p)

varReplace :: Var t -> ReplaceOpt -> Term t m -> Term t m -> Term t m
varReplace (Var v) opt p r = VarTerm v (Replace opt p r)

(@=) :: Var t -> Term t m2 -> Assign t m2
(Var a) @= b = Assign a b

(|>) :: (Shell t m) => m () -> Term t m -> m ()
c |> t = redir c [Redir Out t]

(|>>) :: (Shell t m) => m () -> Term t m -> m ()
c |>> t = redir c [Redir Append t]

(<|) :: (Shell t m) => m () -> Term t m -> m ()
c <| t = redir c [Redir In t]

(<<|) :: Shell t m => m () -> Term t m -> m ()
c <<| t = redir c [RedirStr t]

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

forCmdVar :: Shell t m => t -> [Term t m] -> (Var t -> m ()) -> m ()
forCmdVar t ts b =
  localVar (Var t @= "") >>= \vt ->
    forCmd t ts (b vt)