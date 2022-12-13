-- | Bash conditional commands.
module System.Shell.Internal.Bash.Cond
  ( CondExpr (..),
    UnaryOp (..),
    BinaryOp (..),
  )
where

import Data.String
import System.Shell.Internal.Printer

-- | Bash conditional expressions.
data CondExpr a
  = Unary UnaryOp a
  | Binary a BinaryOp a
  | Not (CondExpr a)
  | And (CondExpr a) (CondExpr a)
  | Or (CondExpr a) (CondExpr a)
  deriving (Eq, Show)

instance Pretty a t => Pretty (CondExpr a) t where
  pretty = go (0 :: Int)
    where
      go _ (Unary op a) = pretty op <+> pretty a
      go _ (Binary a op b) = pretty a <+> pretty op <+> pretty b
      go _ (Not e) = "!" <+> go 2 e
      go p (And e1 e2) = paren (p > 1) $ go 1 e1 <+> "&&" <+> go 1 e2
      go p (Or e1 e2) = paren (p > 0) $ go 0 e1 <+> "||" <+> go 0 e2

      paren False d = d
      paren True d = "(" <+> d <+> ")"

-- | Unary conditional operators.
data UnaryOp
  = -- | @-b@
    BlockFile
  | -- | @-c@
    CharacterFile
  | -- | @-d@
    Directory
  | -- | @-e@, @-a@
    FileExists
  | -- | @-f@
    RegularFile
  | -- | @-g@
    SetGID
  | -- | @-k@
    Sticky
  | -- | @-p@
    NamedPipe
  | -- | @-r@
    Readable
  | -- | @-s@
    FileSize
  | -- | @-t@
    Terminal
  | -- | @-u@
    SetUID
  | -- | @-w@
    Writable
  | -- | @-x@
    Executable
  | -- | @-G@
    GroupOwned
  | -- | @-L@, @-h@
    SymbolicLink
  | -- | @-N@
    Modified
  | -- | @-O@
    UserOwned
  | -- | @-S@
    Socket
  | -- | @-o@
    Optname
  | -- | @-v@
    Varname
  | -- | @-z@
    ZeroString
  | -- | @-n /string/@ or @/string/@
    NonzeroString
  deriving (Eq, Show)

instance (IsString t) => Pretty UnaryOp t where
  pretty t =
    text "-"
      <> text
        ( case t of
            BlockFile -> "b"
            CharacterFile -> "c"
            Directory -> "d"
            FileExists -> "e"
            RegularFile -> "f"
            SetGID -> "g"
            Sticky -> "k"
            NamedPipe -> "p"
            Readable -> "r"
            FileSize -> "s"
            Terminal -> "t"
            SetUID -> "u"
            Writable -> "w"
            Executable -> "x"
            GroupOwned -> "G"
            SymbolicLink -> "L"
            Modified -> "N"
            UserOwned -> "O"
            Socket -> "S"
            Optname -> "o"
            Varname -> "v"
            ZeroString -> "z"
            NonzeroString -> "n"
        )

-- | Binary conditional operators.
data BinaryOp
  = -- | @-ef@
    SameFile
  | -- | @-nt@
    NewerThan
  | -- | @-ot@
    OlderThan
  | -- | @=~@
    StrMatch
  | -- | @==@, @=@
    StrEQ
  | -- | @!=@
    StrNE
  | -- | @<@
    StrLT
  | -- | @>@
    StrGT
  | -- | @-eq@
    ArithEQ
  | -- | @-ne@
    ArithNE
  | -- | @-lt@
    ArithLT
  | -- | @-le@
    ArithLE
  | -- | @-gt@
    ArithGT
  | -- | @-ge@
    ArithGE
  deriving (Eq, Show)

instance (IsString t) => Pretty BinaryOp t where
  pretty t =
    text
      ( case t of
          SameFile -> "-ef"
          NewerThan -> "-nt"
          OlderThan -> "-ot"
          StrMatch -> "=~"
          StrEQ -> "=="
          StrNE -> "!="
          StrLT -> "<"
          StrGT -> ">"
          ArithEQ -> "-eq"
          ArithNE -> "-ne"
          ArithLT -> "-lt"
          ArithLE -> "-le"
          ArithGT -> "-gt"
          ArithGE -> "-ge"
      )
