{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module System.Shell.Internal.Shell
  ( Var (..),
    Arith (..),
    Term (..),
    var,
    output,
    quote,
    quoteTerms,
    Assign (..),
    RedirOp (..),
    Redir (..),
    Test (..),
    AndOrTerm (..),
    AndOrCmd (..),
    ShellStr (..),
    Shell (..),
  )
where

import Data.String
import System.Shell.Str

newtype Var t = Var {varName :: t}

instance (IsString t) => IsString (Var t) where
  fromString = Var . fromString
  {-# INLINE fromString #-}

data Arith t
  = ANum Int
  | AVar (Var t)
  | ANegate (Arith t)
  | APlus (Arith t) (Arith t)
  | AMinus (Arith t) (Arith t)
  | AMult (Arith t) (Arith t)
  | ADiv (Arith t) (Arith t)
  | AMod (Arith t) (Arith t)
  | ANot (Arith t)
  | AOr (Arith t) (Arith t)
  | AAnd (Arith t) (Arith t)
  | AEqual (Arith t) (Arith t)
  | ANotEqual (Arith t) (Arith t)
  | ALT (Arith t) (Arith t)
  | AGT (Arith t) (Arith t)
  | ALE (Arith t) (Arith t)
  | AGE (Arith t) (Arith t)
  | ABitOr (Arith t) (Arith t)
  | ABitXOr (Arith t) (Arith t)
  | ABitAnd (Arith t) (Arith t)
  | AShiftLeft (Arith t) (Arith t)
  | AShiftRight (Arith t) (Arith t)
  | AIf (Arith t) (Arith t, Arith t)

instance Num (Arith t) where
  (+) = APlus
  (*) = AMult
  abs t = (t `ALT` 0) `AIf` (-t, t)
  signum t =
    (t `ALE` 0)
      `AIf` (-1, (t `AEqual` 0) `AIf` (0, 1))
  negate = ANot
  fromInteger = ANum . fromInteger

data Term t m
  = StrTerm t
  | VarTerm t
  | EmptyTerm
  | ArithTerm (Arith t)
  | OutputTerm (m ())
  | QuotedTerm [Term t m]
  | ConcatTerm [Term t m]

instance (IsString t) => IsString (Term t m) where
  fromString s = QuotedTerm [StrTerm (fromString s)]
  {-# INLINE fromString #-}

instance Semigroup (Term t m) where
  ConcatTerm l <> ConcatTerm r = ConcatTerm (l ++ r)
  ConcatTerm l <> r = ConcatTerm (l ++ [r])
  l <> ConcatTerm r = ConcatTerm (l : r)
  l <> r = ConcatTerm [l, r]

instance Monoid (Term t m) where
  mempty = EmptyTerm

newtype TermBuilder t m = TB {getTB :: [Term t m] -> [Term t m]}

instance (IsString t) => IsString (TermBuilder t m) where
  fromString s = TB (fromString s :)
  {-# INLINE fromString #-}

instance Semigroup (TermBuilder t m) where
  TB l <> TB r = TB (l . r)

instance Monoid (TermBuilder t m) where
  mempty = TB id

instance (ShellStr t) => ShellStr (Term t m) where
  type Builder (Term t m) = TermBuilder t m
  fromStr s = TB (s :)
  toStr b = case getTB b [] of
    [] -> EmptyTerm
    [x] -> x
    x -> ConcatTerm x

var :: Var t -> Term t m
var = VarTerm . varName

output :: m () -> Term t m
output = OutputTerm

quote :: Term t m -> Term t m
quote t@(QuotedTerm _) = t
quote t = QuotedTerm [t]

quoteTerms :: [Term t m] -> Term t m
quoteTerms ts =
  QuotedTerm
    ( foldMap
        ( \case
            QuotedTerm t -> t
            t -> [t]
        )
        ts
    )

data RedirOp = In | Out | Append

data Redir t m = Redir
  { redirOp :: RedirOp,
    redirTarget :: Term t m
  }

data Assign t m = Assign t (Term t m)

data Test t m
  = TNot (Test t m)
  | TAnd (Test t m) (Test t m)
  | TOr (Test t m) (Test t m)
  | TFileExists (Term t m)
  | TDirExists (Term t m)
  | TSymbolicLink (Term t m)

data AndOrTerm m
  = AoCmd (m ())
  | AoNot (m ())

data AndOrCmd m
  = AndOrTerm (AndOrTerm m)
  | AndCmd (AndOrTerm m) (AndOrCmd m)
  | OrCmd (AndOrTerm m) (AndOrCmd m)

class (ShellStr t, Monad m) => Shell t m | m -> t where
  test :: Test t m -> m ()
  setVar :: Assign t m -> m ()
  unsetVars :: [t] -> m ()
  runCmd :: [Assign t m] -> Term t m -> [Term t m] -> m ()
  redir :: m () -> [Redir t m] -> m ()
  group :: m () -> m ()
  localVars :: [Assign t m] -> m ()
  newEnvs :: [Assign t m] -> m ()
  pipe :: [m ()] -> m ()
  arithCmd :: Arith t -> m ()
  andOrCmd :: AndOrCmd m -> m ()
  ifCmd :: m () -> m () -> m () -> m ()
  forCmd :: t -> [Term t m] -> m () -> m ()
  whileCmd :: m () -> m () -> m ()
  funcDef :: t -> m a -> m (Term t m)
  script :: Maybe t -> m () -> t