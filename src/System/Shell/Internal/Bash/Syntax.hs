{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Shell script types.
module System.Shell.Internal.Bash.Syntax
  ( -- * Commands
    Command (..),
    ShellCommand (..),
    WordList (..),
    CaseClause (..),
    CaseTerm (..),

    -- * Redirections
    Redir (..),
    IODesc (..),
    RedirOp (..),
    HeredocOp (..),

    -- * Lists
    List (..),
    Statement (..),
    ListTerm (..),
    AndOr (..),
    Pipeline (..),

    -- * Assignments
    Assign (..),
    AssignOp (..),
    RValue (..),
  )
where

import Data.List (intersperse)
import Data.String
import System.Shell.Internal.Bash.Cond (CondExpr)
import System.Shell.Internal.Bash.Pretty
import System.Shell.Internal.Bash.Word
import System.Shell.Internal.Printer
import Prelude hiding (Word)

-- | The BashDoc monoid is used for building Statements, AndOr or Pipelines.
-- Consider the following situation: We have the following command
--
-- > cat <<EOF
-- > some here doc
-- > EOF
--
-- and we want to pipe its output to another arbitrary command @cmd@.
-- We want this pipeline to look like this:
--
-- > cat <<EOF |
-- > some here doc
-- > EOF
-- > cmd
--
-- Note the @|@ at the end of the first line: If we were simply pretty printing the @cat@ command we had no idea where to insert the pipe symbol.
-- And that's the purpose of BashDoc: We store possible suffixes to such lines, commands and the here documents attached to them separately and do the concatenation in the Semigroup instance of BashDoc.
data BashDoc ann
  = BashDoc
      (Doc ann)
      -- ^ The head: This is stuff we want to put before the line break and here documents
      (Doc ann)
      -- ^ The tail: Everthing which follows the here documents
      (Doc ann)
      -- ^ Collected here documents

instance (IsString ann) => Semigroup (BashDoc ann) where
  BashDoc Empty Empty Empty <> y = y
  x <> BashDoc Empty Empty Empty = x
  BashDoc h1 t1 Empty <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> h2 <++> t2) hds2
  BashDoc h1 t1 hds1 <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> noIndent (h2 $++$ hds1) $++$ t2) hds2
    where
      noIndent doc = nesting $ \i -> nest (-i) doc

instance (IsString ann) => Monoid (BashDoc ann) where
  mempty = BashDoc mempty mempty mempty
  mappend = (<>)

docOp :: Doc ann -> BashDoc ann
docOp xs = BashDoc xs mempty mempty

prettyBashDoc :: (IsString ann) => BashDoc ann -> Doc ann
prettyBashDoc (BashDoc h t hds) = h <++> t $++$ hds

-- | A utility class for pretty printing without heredocs
class ToBashDoc a t where
  toBashDoc :: a -> BashDoc t

prettyHeredocs :: (IsString t) => [Redir t] -> Doc t
prettyHeredocs [] = mempty
prettyHeredocs rs = mconcat $ intersperse line $ map prettyHeredoc rs
  where
    prettyHeredoc Heredoc {..} = pretty hereDocument <> text heredocDelim
    prettyHeredoc _ = mempty

-- | Indent by 4 columns.
indent' :: Doc ann -> Doc ann
indent' = indent 4

-- | Render a conditional command with a block.
prettyBlock :: (IsString ann) => Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
prettyBlock pre cond bs block be = pre <+> cond <+> bs $+$ block $+$ be

-- | Render a conditional command with a block whose condition is a list of statements.
prettyBlockList :: (IsString ann) => Doc ann -> List ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
prettyBlockList pre l bs block be
  | hasHeredoc l = pre <+> pretty l $+$ bs $+$ block $+$ be
  | otherwise = prettyBlock pre (pretty l) bs block be

-- | Does the last statement in a list have a here doc attached?
hasHeredoc :: forall t. (IsString t) => List t -> Bool
hasHeredoc (List []) = False
hasHeredoc (List xs) =
  let Statement l _ = last xs
      BashDoc _ _ hds = toBashDoc @(AndOr t) @t l
   in case hds of
        Empty -> False
        _ -> True

-- | A Bash command with redirections.
data Command t = Command (ShellCommand t) [Redir t]
  deriving (Eq, Show)

instance (IsString t) => Pretty (Command t) t where
  pretty = prettyBashDoc . toBashDoc

instance (IsString t) => ToBashDoc (Command t) t where
  toBashDoc (Command c rs) = BashDoc mempty (pretty c <++> prettyList rs) (prettyHeredocs $ filter isHeredoc rs)
    where
      isHeredoc Heredoc {} = True
      isHeredoc _ = False

-- | A Bash command.
data ShellCommand t
  = -- | A simple command consisting of assignments followed by words.
    SimpleCommand [Assign t] [Word t]
  | -- | The shell builtins @declare@, @eval@, @export@, @local@, @readonly@,
    -- and @typeset@ can accept both assignments and words as arguments.
    AssignBuiltin (Word t) [Either (Assign t) (Word t)]
  | -- | A function name and definition.
    FunctionDef t (List t)
  | -- | A named coprocess.
    Coproc t (Command t)
  | -- | A @(...)@ list, denoting a subshell.
    Subshell (List t)
  | -- | A @{...}@ list.
    Group (List t)
  | -- | An arithmetic expression.
    Arith t
  | -- | A Bash @[[...]]@ conditional expression.
    Cond (CondExpr (Word t))
  | -- | A @for /name/ in /words/@ command. If @in /words/@ is absent,
    -- the word list defaults to @\"$\@\"@.
    For t (WordList t) (List t)
  | -- | An arithmetic @for ((...))@ command.
    ArithFor t (List t)
  | -- | A @select /name/ in /words/@ command. If @in /words/@ is absent,
    -- the word list defaults to @\"$\@\"@.
    Select t (WordList t) (List t)
  | -- | A @case@ command.
    Case (Word t) [CaseClause t]
  | -- | An @if@ command, with a predicate, consequent, and alternative.
    -- @elif@ clauses are parsed as nested @if@ statements.
    If (List t) (List t) (Maybe (List t))
  | -- | An @until@ command.
    Until (List t) (List t)
  | -- | A @while@ command.
    While (List t) (List t)
  deriving (Eq, Show)

instance (IsString t) => Pretty (ShellCommand t) t where
  pretty (SimpleCommand as ws) = prettyList as <++> pretty ws
  pretty (AssignBuiltin w args) = pretty w <++> hsep (map (either pretty pretty) args)
  pretty (FunctionDef name l) =
    text name <+> "()" $+$ pretty (Group l)
  pretty (Coproc name c) =
    "coproc" <+> text name <+> pretty c
  pretty (Subshell l) =
    "(" <+> pretty l <+> ")"
  pretty (Group l) =
    "{" $+$ indent' (pretty l) $+$ "}"
  pretty (Arith s) =
    "((" <> text s <> "))"
  pretty (Cond e) =
    "[[" <+> pretty e <+> "]]"
  pretty (For w ws l) =
    prettyBlock "for" (text w <+> pretty ws <> ";") "do" (indent' $ pretty l) "done"
  pretty (ArithFor s l) =
    prettyBlock "for" ("((" <> text s <> "))") "do" (indent' $ pretty l) "done"
  pretty (Select w ws l) =
    prettyBlock "select" (text w <++> pretty ws <> ";") "do" (indent' $ pretty l) "done"
  pretty (Case w cs) =
    prettyBlock "case" (pretty w) "in" (vcat $ map (indent' . pretty) cs) "esac"
  pretty (If p t f) =
    prettyBlockList
      "if"
      p
      "then"
      ( indent' (pretty t) $++$ maybe mempty (\l -> "else" $+$ indent' (pretty l)) f
      )
      "fi"
  pretty (Until p l) =
    prettyBlockList "until" p "do" (indent' $ pretty l) "done"
  pretty (While p l) =
    prettyBlockList "while" p "do" (indent' $ pretty l) "done"

-- | A word list or @\"$\@\"@.
data WordList t
  = Args
  | WordList [Word t]
  deriving (Eq, Show)

instance (IsString t) => Pretty (WordList t) t where
  pretty Args = mempty
  pretty (WordList ws) = "in" <+> pretty ws

-- | A single case clause.
data CaseClause t = CaseClause [Word t] (List t) CaseTerm
  deriving (Eq, Show)

instance (IsString t) => Pretty (CaseClause t) t where
  pretty (CaseClause ps l term) =
    hcat (punctuate " | " (map pretty ps)) <> ")"
      $+$ indent' (pretty l)
      $+$ (indent' $ pretty term)

-- | A case clause terminator.
data CaseTerm
  = -- | @;;@
    Break
  | -- | @;&@
    FallThrough
  | -- | @;;&@
    Continue
  deriving (Eq, Show)

instance (IsString t) => Pretty CaseTerm t where
  pretty Break = ";;"
  pretty FallThrough = ";&"
  pretty Continue = ";;&"

-- | A redirection.
data Redir t
  = -- | A redirection.
    Redir
      { -- | An optional file descriptor.
        redirDesc :: Maybe (IODesc t),
        -- | The redirection operator.
        redirOp :: RedirOp,
        -- | The redirection target.
        redirTarget :: Word t
      }
  | -- | A here document.
    Heredoc
      { -- | The here document operator.
        heredocOp :: HeredocOp,
        -- | The here document delimiter.
        heredocDelim :: t,
        -- | 'True' if the delimiter was quoted.
        heredocDelimQuoted :: Bool,
        -- | The document itself, if the delimiter was quoted, no expansions
        -- are parsed. If the delimiter was not quoted, parameter, arithmetic
        -- and command substitutions take place.
        hereDocument :: Word t
      }
  deriving (Eq, Show)

instance (IsString t) => Pretty (Redir t) t where
  pretty Redir {..} =
    pretty redirDesc <> pretty redirOp <> pretty redirTarget
  pretty Heredoc {..} =
    pretty heredocOp
      <> ( if heredocDelimQuoted
             then "'" <> text heredocDelim <> "'"
             else text heredocDelim
         )

  prettyList = hsep . map pretty

-- | A redirection file descriptor.
data IODesc t
  = -- | A file descriptor number.
    IONumber Int
  | -- | A variable @{/varname/}@ to allocate a file descriptor for.
    IOVar t
  deriving (Eq, Show)

instance (IsString t) => Pretty (IODesc t) t where
  pretty (IONumber n) = pretty n
  pretty (IOVar n) = "{" <> text n <> "}"

-- | A redirection operator.
data RedirOp
  = -- | @\<@
    In
  | -- | @\>@
    Out
  | -- | @\>|@
    OutOr
  | -- | @\>\>@
    Append
  | -- | @&\>@
    AndOut
  | -- | @&\>\>@
    AndAppend
  | -- | @\<\<\<@
    HereString
  | -- | @\<&@
    InAnd
  | -- | @\>&@
    OutAnd
  | -- | @\<\>@
    InOut
  deriving (Eq, Show)

instance (IsString t) => Pretty RedirOp t where
  pretty t = case t of
    In -> "<"
    Out -> ">"
    OutOr -> ">|"
    Append -> ">>"
    AndOut -> "&>"
    AndAppend -> "&>>"
    HereString -> "<<<"
    InAnd -> "<&"
    OutAnd -> ">&"
    InOut -> "<>"

-- | A here document operator.
data HeredocOp
  = -- | @\<\<@
    Here
  | -- | @\<\<-@
    HereStrip
  deriving (Eq, Show)

instance (IsString t) => Pretty HeredocOp t where
  pretty o = case o of
    Here -> "<<"
    HereStrip -> "<<-"

-- | A compound list of statements.
newtype List t = List [Statement t]
  deriving (Eq, Show)

instance (IsString t) => Pretty (List t) t where
  pretty (List as) = prettyList as

-- | A single statement in a list.
data Statement t = Statement (AndOr t) ListTerm
  deriving (Eq, Show)

instance (IsString t) => Pretty (Statement t) t where
  pretty = prettyBashDoc . toBashDoc

  prettyList = foldr f mempty
    where
      f a@(Statement _ Sequential) b = pretty a $++$ b
      f a@(Statement _ Asynchronous) b = pretty a <++> b

instance (IsString t) => ToBashDoc (Statement t) t where
  toBashDoc (Statement l lt) = toBashDoc l <> toBashDoc lt

-- | A statement terminator.
data ListTerm
  = -- | @;@
    Sequential
  | -- | @&@
    Asynchronous
  deriving (Eq, Show)

instance (IsString t) => Pretty ListTerm t where
  pretty t = case t of
    Sequential -> ";"
    Asynchronous -> "&"

instance (IsString t) => ToBashDoc ListTerm t where
  toBashDoc Sequential = docOp ";"
  toBashDoc Asynchronous = docOp "&"

-- | A right-associative list of pipelines.
data AndOr t
  = -- | The last pipeline of a list.
    Last (Pipeline t)
  | -- | A @&&@ construct.
    And (Pipeline t) (AndOr t)
  | -- | A @||@ construct.
    Or (Pipeline t) (AndOr t)
  deriving (Eq, Show)

instance (IsString t) => Pretty (AndOr t) t where
  pretty = prettyBashDoc . toBashDoc

instance (IsString t) => ToBashDoc (AndOr t) t where
  toBashDoc (Last p) = toBashDoc p
  toBashDoc (And p a) = toBashDoc p <> docOp " &&" <> toBashDoc a
  toBashDoc (Or p a) = toBashDoc p <> docOp " ||" <> toBashDoc a

-- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
data Pipeline t = Pipeline
  { -- | 'True' if the pipeline is timed with @time@.
    timed :: Bool,
    -- | 'True' if the pipeline is timed with the @-p@ flag.
    timedPosix :: Bool,
    -- | 'True' if the pipeline is inverted with @!@.
    inverted :: Bool,
    -- | A list of commands, separated by @|@, or @|&@.
    -- @command1 |& command2@ is treated as a shorthand for
    -- @command1 2>&1 | command2@.
    commands :: [Command t]
  }
  deriving (Eq, Show)

instance (IsString t) => Pretty (Pipeline t) t where
  pretty = prettyBashDoc . toBashDoc

instance (IsString t) => ToBashDoc (Pipeline t) t where
  toBashDoc Pipeline {..} =
    let timed' = if timed then "time" else mempty
        timedPosix' = if timedPosix then "-p" else mempty
        inverted' = if inverted then "!" else mempty
        space = if timed || timedPosix || inverted then " " else mempty
        prefix = BashDoc mempty (timed' <++> timedPosix' <++> inverted' <> space) mempty
     in prefix <> mconcat (intersperse (docOp " |") (map toBashDoc commands))

-- | An assignment.
data Assign t = Assign (Parameter t) AssignOp (RValue t)
  deriving (Eq, Show)

instance (IsString t) => Pretty (Assign t) t where
  pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

  prettyList = hsep . map pretty

-- | An assignment operator.
data AssignOp
  = -- | @=@
    Equals
  | -- | @+=@
    PlusEquals
  deriving (Eq, Show)

instance (IsString t) => Pretty AssignOp t where
  pretty t = case t of
    Equals -> "="
    PlusEquals -> "+="

-- | The right side of an assignment.
data RValue t
  = -- | A simple word.
    RValue (Word t)
  | -- | An array assignment, as @(subscript, word)@ pairs.
    RArray [(Maybe (Word t), Word t)]
  deriving (Eq, Show)

instance (IsString t) => Pretty (RValue t) t where
  pretty (RValue w) = pretty w
  pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
    where
      f (Nothing, w) = pretty w
      f (Just sub, w) = "[" <> pretty sub <> "]=" <> pretty w
