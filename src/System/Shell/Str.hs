{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module System.Shell.Str (ShellStr (..)) where

import Data.String

class
  ( IsString t,
    Monoid t,
    IsString (Builder t),
    Monoid (Builder t)
  ) =>
  ShellStr t
  where
  type Builder t
  fromStr :: t -> Builder t
  toStr :: Builder t -> t

newtype StringBuilder = SB {runSB :: String -> String}

instance IsString StringBuilder where
  fromString s = SB (s ++)

instance Semigroup StringBuilder where
  SB l <> SB r = SB (l . r)

instance Monoid StringBuilder where
  mempty = SB id

instance ShellStr String where
  type Builder String = StringBuilder
  fromStr = fromString
  toStr s = runSB s []
