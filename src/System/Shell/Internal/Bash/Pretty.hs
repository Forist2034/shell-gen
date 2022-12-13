-- | Pretty-printing of Bash scripts. This tries to stay close to the format
-- used by the Bash builtin @declare -f@.
module System.Shell.Internal.Bash.Pretty
  ( ($+$),
    ($++$),
    (<++>),
    prettyText,
  )
where

import Data.String
import System.Shell.Internal.Printer
import System.Shell.Str

-- | @x $+$ y@ concatenates @x@ and @y@ with a 'line' in between
($+$) :: Doc ann -> Doc ann -> Doc ann
x $+$ y = x <> line <> y

-- | Behaves like '$+$' except that if one of the documents was empty we do not concatenate at all.
-- 'mempty' is the identity of '$+$':
--
-- prop> x $++$ mempty == x
--
-- and
--
-- prop> mempty $++$ y == y
($++$) :: Doc ann -> Doc ann -> Doc ann
Empty $++$ y = y
x $++$ Empty = x
x $++$ y = x <> line <> y

-- | Behaves like '<+>' except that if one of the documents was empty we do not concatenate at all.
-- 'mempty' is the identity of '<+>':
--
-- prop> x <++> mempty == x
--
-- and
--
-- prop> mempty <++> y == y
(<++>) :: (IsString t) => Doc t -> Doc t -> Doc t
Empty <++> y = y
x <++> Empty = x
x <++> y = x <+> y

-- | Pretty-print to a 'String'.
prettyText :: (ShellStr t, Pretty a t) => a -> t
prettyText = renderDoc . pretty
