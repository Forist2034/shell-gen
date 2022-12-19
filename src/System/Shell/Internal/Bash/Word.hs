{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Bash words and substitutions.
module System.Shell.Internal.Bash.Word
  ( -- * Words
    Word,
    Span (..),

    -- * Parameters
    Parameter (..),
    ParamSubst (..),
    AltOp (..),
    LetterCaseOp (..),
    Direction (..),

    -- * Process
    ProcessSubstOp (..),

    -- * Manipulation
    stringToWord,
  )
where

import Data.String
import System.Shell.Internal.Printer
import Prelude hiding (Word)

-- | A Bash word, broken up into logical spans.
type Word t = [Span t]

-- | An individual unit of a word.
data Span t
  = -- | normal string
    Str t
  | -- | An escaped character.
    Escape Char
  | -- | A single-quoted string.
    Single (Word t)
  | -- | A double-quoted string.
    Double (Word t)
  | -- | A ANSI C string.
    ANSIC (Word t)
  | -- | A locale-translated string.
    Locale (Word t)
  | -- | A backquote-style command substitution.
    -- To extract the command string, 'unquote' the word inside.
    Backquote (Word t)
  | -- | A parameter substitution.
    ParamSubst (ParamSubst t)
  | -- | An arithmetic substitution.
    ArithSubst t
  | -- | A command substitution.
    CommandSubst t
  | -- | A process substitution.
    ProcessSubst ProcessSubstOp t
  deriving (Eq, Show)

instance (IsString t) => Pretty (Span t) t where
  pretty (Str c) = text c
  pretty (Escape c) = "\\" <> fromString [c]
  pretty (Single w) = "\'" <> pretty w <> "\'"
  pretty (Double w) = "\"" <> pretty w <> "\""
  pretty (ANSIC w) = "$\'" <> pretty w <> "\'"
  pretty (Locale w) = "$\"" <> pretty w <> "\""
  pretty (Backquote w) = "`" <> pretty w <> "`"
  pretty (ParamSubst s) = pretty s
  pretty (ArithSubst s) = "$((" <> text s <> "))"
  pretty (CommandSubst s) = "$(" <> text s <> ")"
  pretty (ProcessSubst c s) = pretty c <> "(" <> text s <> ")"

instance (IsString t) => Pretty [Span t] t where
  pretty = hsep . fmap pretty

instance (IsString t) => Pretty [[Span t]] t where
  pretty = prettyList

-- | A parameter name an optional subscript.
data Parameter t = Parameter t (Maybe (Word t))
  deriving (Eq, Show)

instance (IsString t) => Pretty (Parameter t) t where
  pretty (Parameter s sub) = text s <> subscript sub
    where
      subscript Nothing = mempty
      subscript (Just w) = "[" <> pretty w <> "]"

-- | A parameter substitution.
data ParamSubst t
  = Bare
      { -- | The parameter to substitute.
        parameter :: Parameter t
      }
  | Brace
      { -- | Use indirect expansion.
        indirect :: Bool,
        parameter :: Parameter t
      }
  | Alt
      { indirect :: Bool,
        parameter :: Parameter t,
        -- | Test for both existence and null values.
        testNull :: Bool,
        -- | The operator.
        altOp :: AltOp,
        -- | The alternate word.
        altWord :: Word t
      }
  | Substring
      { indirect :: Bool,
        parameter :: Parameter t,
        -- | The substring offset.
        subOffset :: Word t,
        -- | The substring length, if any.
        subLength :: Word t
      }
  | Prefix
      { -- | The variable prefix.
        prefix :: t,
        -- | Either @\@@ of @*@.
        modifier :: Char
      }
  | Indices
      { parameter :: Parameter t
      }
  | Length
      { parameter :: Parameter t
      }
  | Delete
      { indirect :: Bool,
        parameter :: Parameter t,
        -- | Replace the longest match instead of the shortest match.
        longest :: Bool,
        -- | Where to delete from.
        deleteDirection :: Direction,
        -- | The replacement pattern.
        pattern :: Word t
      }
  | Replace
      { indirect :: Bool,
        parameter :: Parameter t,
        -- | Replace all occurences.
        replaceAll :: Bool,
        -- | Where to replace.
        replaceDirection :: Maybe Direction,
        pattern :: Word t,
        -- | The replacement string.
        replacement :: Word t
      }
  | LetterCase
      { indirect :: Bool,
        parameter :: Parameter t,
        -- | Convert to lowercase, not uppercase.
        letterCaseOp :: LetterCaseOp,
        -- | Convert all characters, not only the starts of words.
        convertAll :: Bool,
        pattern :: Word t
      }
  deriving (Eq, Show)

prettyParameter :: (IsString t) => Bool -> Parameter t -> Doc t -> Doc t
prettyParameter bang param suffix =
  "${" <> (if bang then "!" else mempty) <> pretty param <> suffix <> "}"

twiceWhen :: Bool -> Doc t -> Doc t
twiceWhen False d = d
twiceWhen True d = d <> d

instance (IsString t) => Pretty (ParamSubst t) t where
  pretty Bare {..} = "$" <> pretty parameter
  pretty Brace {..} = prettyParameter indirect parameter mempty
  pretty Alt {..} =
    prettyParameter indirect parameter $
      (if testNull then ":" else mempty)
        <> pretty altOp
        <> pretty altWord
  pretty Substring {..} =
    prettyParameter indirect parameter $
      ":"
        <> pretty subOffset
        <> (if null subLength then mempty else ":")
        <> pretty subLength
  pretty Prefix {..} = "${!" <> text prefix <> pretty modifier <> "}"
  pretty Indices {..} = prettyParameter True parameter mempty
  pretty Length {..} = "${#" <> pretty parameter <> "}"
  pretty Delete {..} =
    prettyParameter indirect parameter $
      twiceWhen longest (pretty deleteDirection)
        <> pretty pattern
  pretty Replace {..} =
    prettyParameter indirect parameter $
      "/"
        <> (if replaceAll then "/" else mempty)
        <> pretty replaceDirection
        <> pretty pattern
        <> "/"
        <> pretty replacement
  pretty LetterCase {..} =
    prettyParameter indirect parameter $
      twiceWhen convertAll (pretty letterCaseOp)
        <> pretty pattern

-- | An alternation operator.
data AltOp
  = -- | '-', ':-'
    AltDefault
  | -- | '=', ':='
    AltAssign
  | -- | '?', ':?'
    AltError
  | -- | '+', ':+'
    AltReplace
  deriving (Eq, Ord, Read, Show)

instance (IsString t) => Pretty AltOp t where
  pretty AltDefault = "-"
  pretty AltAssign = "="
  pretty AltError = "?"
  pretty AltReplace = "+"

-- | A letter case operator.
data LetterCaseOp
  = ToLower
  | ToUpper
  deriving (Eq, Show)

instance (IsString t) => Pretty LetterCaseOp t where
  pretty ToLower = ","
  pretty ToUpper = "^"

-- | A string direction.
data Direction
  = Front
  | Back
  deriving (Eq, Show)

instance (IsString t) => Pretty Direction t where
  pretty Front = "#"
  pretty Back = "%"

-- | A process substitution.
data ProcessSubstOp
  = -- | @\<@
    ProcessIn
  | -- | @\>@
    ProcessOut
  deriving (Eq, Show)

instance (IsString t) => Pretty ProcessSubstOp t where
  pretty ProcessIn = text "<"
  pretty ProcessOut = text ">"

-- | Convert a string to an unquoted word.
stringToWord :: t -> Word t
stringToWord t = [Str t]