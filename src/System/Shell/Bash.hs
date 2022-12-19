{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Shell.Bash
  ( Quoted (..),
    quoteChar,
    Quotable (..),
    BashScript,
  )
where

import Control.Monad.Reader
import Control.Monad.Writer hiding (Last)
import Data.String
import System.Shell.Internal.Bash.Cond as C
import System.Shell.Internal.Bash.Syntax as S
import System.Shell.Internal.Bash.Word
import System.Shell.Internal.Printer
import qualified System.Shell.Internal.Shell as SH
import System.Shell.Str
import Prelude hiding (Word)

data Quoted t
  = QStr t
  | QEscape Char
  deriving (Show)

quoteChar :: Char -> Bool
quoteChar c = c == '$' || c == '\\' || c == '`' || c == '"'

class Quotable t where
  quote :: t -> [Quoted t]

instance Quotable String where
  quote [] = []
  quote (x : xs)
    | quoteChar x = QEscape x : quote xs
    | otherwise = case quote xs of
        QStr s : qs -> QStr (x : s) : qs
        qs -> QStr [x] : qs

newtype Cmd t = Cmd {cmds :: [Command t]}

instance Semigroup (Cmd t) where
  Cmd l <> Cmd r = Cmd (r ++ l)

instance Monoid (Cmd t) where
  mempty = Cmd []

newtype BashScript t a = BS (ReaderT Bool (Writer (Cmd t)) a)
  deriving (Functor, Applicative, Monad)

runBS :: Bool -> BashScript t a -> [Command t]
runBS func (BS s) = reverse (cmds (execWriter (runReaderT s func)))

arithToStr :: (ShellStr t) => SH.Arith t -> t
arithToStr t = toStr (conv True t)
  where
    paren b v = if b then v else "( " <> v <> " )"
    convF = conv False
    binary top a o b = paren top (convF a <> " " <> o <> " " <> convF b)

    conv _ (SH.ANum n) = fromString (show n)
    conv _ (SH.AVar (SH.Var v)) = fromStr v
    conv top (SH.ANegate a) = paren top ("-" <> convF a)
    conv top (SH.APlus a b) = binary top a "+" b
    conv top (SH.AMinus a b) = binary top a "-" b
    conv top (SH.AMult a b) = binary top a "*" b
    conv top (SH.ADiv a b) = binary top a "/" b
    conv top (SH.AMod a b) = binary top a "%" b
    conv top (SH.ANot a) = paren top ("! " <> convF a)
    conv top (SH.AOr a b) = binary top a "||" b
    conv top (SH.AAnd a b) = binary top a "&&" b
    conv top (SH.AEqual a b) = binary top a "==" b
    conv top (SH.ANotEqual a b) = binary top a "!=" b
    conv top (SH.ALT a b) = binary top a "<" b
    conv top (SH.AGT a b) = binary top a ">" b
    conv top (SH.ALE a b) = binary top a "<=" b
    conv top (SH.AGE a b) = binary top a ">=" b
    conv top (SH.ABitOr a b) = binary top a "|" b
    conv top (SH.ABitXOr a b) = binary top a "^" b
    conv top (SH.ABitAnd a b) = binary top a "&" b
    conv top (SH.AShiftLeft a b) = binary top a "<<" b
    conv top (SH.AShiftRight a b) = binary top a ">>" b
    conv top (SH.AIf c (l, r)) = paren top (convF c <> " ? " <> convF l <> " : " <> convF r)

termToWord :: (SH.Shell t m, Quotable t) => SH.Term t m -> Word t
termToWord (SH.StrTerm t) = [Str t]
termToWord (SH.ArithTerm at) = [ArithSubst (arithToStr at)]
termToWord SH.EmptyTerm = []
termToWord (SH.VarTerm v t) = case t of
  SH.NormalVar -> [ParamSubst (Brace False (Parameter v Nothing))]
  SH.VarLength -> [ParamSubst (Length (Parameter v Nothing))]
  SH.Suffix o ->
    [ ParamSubst
        ( Substring
            { indirect = False,
              parameter = Parameter v Nothing,
              subOffset = termToWord o,
              subLength = []
            }
        )
    ]
  SH.Substr o l ->
    [ ParamSubst
        ( Substring
            { indirect = False,
              parameter = Parameter v Nothing,
              subOffset = termToWord o,
              subLength = termToWord l
            }
        )
    ]
  SH.TrimPrefix o p ->
    [ ParamSubst
        ( Delete
            { indirect = False,
              parameter = Parameter v Nothing,
              longest = o == SH.LongestM,
              deleteDirection = Front,
              pattern = termToWord p
            }
        )
    ]
  SH.TrimSuffix o p ->
    [ ParamSubst
        ( Delete
            { indirect = False,
              parameter = Parameter v Nothing,
              longest = o == SH.LongestM,
              deleteDirection = Back,
              pattern = termToWord p
            }
        )
    ]
termToWord (SH.OutputTerm o) = [CommandSubst (SH.script Nothing o)]
termToWord (SH.QuotedTerm t) =
  [ Double
      ( foldMap
          ( \case
              SH.StrTerm s -> fmap conv (quote s)
              ts -> termToWord ts
          )
          t
      )
  ]
  where
    conv (QStr s) = Str s
    conv (QEscape c) = Escape c
termToWord (SH.ConcatTerm t) = foldMap termToWord t

convAssign :: (SH.Shell t m, Quotable t) => SH.Assign t m -> Assign t
convAssign (SH.Assign n t) =
  Assign
    (Parameter n Nothing)
    Equals
    (RValue (termToWord t))

convToList :: [Command t] -> List t
convToList cs =
  List
    ( fmap
        ( \c ->
            Statement
              ( Last
                  ( Pipeline
                      { timed = False,
                        timedPosix = False,
                        inverted = False,
                        commands = [c]
                      }
                  )
              )
              Sequential
        )
        cs
    )

pipeline :: [Command t] -> Pipeline t
pipeline c = Pipeline {timed = False, timedPosix = False, inverted = False, commands = c}

instance (SH.ShellStr t, Quotable t) => SH.Shell t (BashScript t) where
  test t = BS (tell (Cmd [Command (Cond (convTest t)) []]))
    where
      convTest :: SH.Test t (BashScript t) -> CondExpr (Word t)
      convTest (SH.TNot t1) = Not (convTest t1)
      convTest (SH.TAnd l r) = C.And (convTest l) (convTest r)
      convTest (SH.TOr l r) = C.Or (convTest l) (convTest r)
      convTest (SH.TRegularFile f) = Unary RegularFile (termToWord f)
      convTest (SH.TFileExists v) =
        Unary
          FileExists
          (termToWord v)
      convTest (SH.TDirExists v) =
        Unary
          Directory
          (termToWord v)
      convTest (SH.TSymbolicLink v) =
        Unary SymbolicLink (termToWord v)
      convTest (SH.TStrEqual l r) =
        Binary (termToWord l) StrEQ (termToWord r)
      convTest (SH.TStrNotEqual l r) =
        Binary (termToWord l) StrNE (termToWord r)
      convTest (SH.TStrMatch l r) =
        Binary (termToWord l) StrMatch (termToWord r)

  setVar v =
    BS
      ( tell
          ( Cmd
              [Command (AssignBuiltin [] [Left (convAssign v)]) []]
          )
      )
  unsetVars ts = SH.runCmd [] "unset" (fmap SH.StrTerm ts)
  runCmd ass cmd arg =
    BS
      ( tell
          ( Cmd
              [ Command
                  ( SimpleCommand
                      (fmap convAssign ass)
                      (termToWord cmd : fmap termToWord arg)
                  )
                  []
              ]
          )
      )
  redir (BS mc) ro =
    BS
      ( censor
          ( \(Cmd v) ->
              Cmd
                ( case v of
                    [] -> []
                    [Command s []] -> [Command s (fmap convRed ro)]
                    cs -> [Command (Group (convToList (reverse cs))) (fmap convRed ro)]
                )
          )
          mc
      )
    where
      convRed (SH.Redir op t) =
        Redir
          { redirDesc = Nothing,
            redirOp = case op of
              SH.In -> In
              SH.Out -> Out
              SH.Append -> Append,
            redirTarget = termToWord t
          }
      convRed (SH.RedirStr s) =
        Redir
          { redirDesc = Nothing,
            redirOp = HereString,
            redirTarget = termToWord s
          }
  group (BS c) =
    BS
      (censor (\(Cmd v) -> Cmd [Command (Group (convToList (reverse v))) []]) c)
  subShell (BS c) =
    BS (censor (\(Cmd v) -> Cmd [Command (Subshell (convToList (reverse v))) []]) c)
  localVars a =
    BS
      ( do
          inFun <- ask
          tell
            ( Cmd
                [ Command
                    ( AssignBuiltin
                        (if inFun then [Str "local"] else [Str "declare"])
                        (fmap (Left . convAssign) a)
                    )
                    []
                ]
            )
      )
  newEnvs a =
    BS
      ( tell
          ( Cmd
              [ Command
                  ( AssignBuiltin
                      [Str "export"]
                      (fmap (Left . convAssign) a)
                  )
                  []
              ]
          )
      )
  ifCmd c t f =
    BS
      ( do
          inF <- ask
          tell
            ( Cmd
                [ Command
                    ( If
                        (convToList (runBS inF c))
                        (convToList (runBS inF t))
                        ( case runBS inF f of
                            [] -> Nothing
                            fs -> Just (convToList fs)
                        )
                    )
                    []
                ]
            )
      )
  forCmd t ts b =
    BS
      ( do
          inF <- ask
          tell
            ( Cmd
                [ Command
                    ( For
                        t
                        (WordList (fmap termToWord ts))
                        (convToList (runBS inF b))
                    )
                    []
                ]
            )
      )
  whileCmd c b =
    BS
      ( do
          inF <- ask
          tell
            ( Cmd
                [ Command
                    ( While
                        (convToList (runBS inF c))
                        (convToList (runBS inF b))
                    )
                    []
                ]
            )
      )
  pipe cs =
    BS
      ( do
          inF <- ask
          tell
            ( Cmd
                [ Command
                    ( Group
                        ( List
                            [ Statement
                                ( Last
                                    (pipeline (foldMap (runBS inF) cs))
                                )
                                Sequential
                            ]
                        )
                    )
                    []
                ]
            )
      )
  arithCmd a = BS (tell (Cmd [Command (Arith (arithToStr a)) []]))
  andOrCmd c =
    BS
      ( do
          inF <- ask
          tell
            ( Cmd
                [ Command
                    ( Group
                        ( List
                            [ Statement
                                (conv inF c)
                                Sequential
                            ]
                        )
                    )
                    []
                ]
            )
      )
    where
      convT f (SH.AoCmd ct) = pipeline (runBS f ct)
      convT f (SH.AoNot ct) =
        (pipeline (runBS f ct))
          { inverted = True
          }
      conv f (SH.AndOrTerm ct) = Last (convT f ct)
      conv f (SH.AndCmd p as) = S.And (convT f p) (conv f as)
      conv f (SH.OrCmd p as) = S.Or (convT f p) (conv f as)
  funcDef name (BS mc) =
    BS
      ( local
          (const True)
          ( censor
              ( \(Cmd cs) ->
                  Cmd
                    [ Command
                        (FunctionDef name (convToList (reverse cs)))
                        []
                    ]
              )
              mc
          )
          >> return (SH.StrTerm name)
      )
  script sh c =
    let cmd = runBS False c
     in renderDoc (maybe Empty (\v -> "#!" <> text v <> line) sh <> vcat (fmap pretty cmd))