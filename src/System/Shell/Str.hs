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
