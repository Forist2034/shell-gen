{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Shell.Internal.Printer
  ( Doc (Empty),
    renderDoc,
    text,
    line,
    indent,
    nest,
    nesting,
    vcat,
    hcat,
    hsep,
    punctuate,
    (<+>),
    Pretty (..),
  )
where

import Data.Bifunctor
import Data.Semigroup
import Data.String
import System.Shell.Str

data Doc t
  = Text t
  | Line
  | Cat (Doc t) (Doc t)
  | Nest Int (Doc t)
  | Indent Int (Doc t)
  | Nesting (Int -> Doc t)
  | Empty

instance Semigroup (Doc t) where
  l <> r = Cat l r

instance Monoid (Doc t) where
  mempty = Empty

instance (IsString t) => IsString (Doc t) where
  fromString t = Text (fromString t)
  {-# INLINE fromString #-}

renderDoc :: forall a. (ShellStr a) => Doc a -> a
renderDoc doc = toStr (fst (go 0 doc))
  where
    spaces n
      | n > 0 = stimes n " "
      | otherwise = mempty

    go :: Int -> Doc a -> (Builder a, Bool)
    go _ (Text t) = (fromStr t, False)
    go _ Line = ("\n", True)
    go a (Cat l r) =
      let (tl, ll) = go a l
          (tr, rr) = go a r
       in if ll
            then (tl <> spaces a <> tr, rr)
            else (tl <> tr, rr)
    go a (Nest n d) = go (max (a + n) 0) d
    go a (Indent n d) = first (spaces n <>) (go (max (a + n) 0) d)
    go a (Nesting f) = go a (f a)
    go _ Empty = (mempty, False)

text :: t -> Doc t
text = Text

line :: Doc t
line = Line

indent :: Int -> Doc t -> Doc t
indent n
  | n <= 0 = id
  | otherwise = Indent n

nest :: Int -> Doc t -> Doc t
nest = Nest

nesting :: (Int -> Doc t) -> Doc t
nesting = Nesting

vcat :: [Doc t] -> Doc t
vcat = foldr (\i d -> i <> Line <> d) Empty

hcat :: [Doc t] -> Doc t
hcat = mconcat

hsep :: (IsString t) => [Doc t] -> Doc t
hsep [] = Empty
hsep x@(_ : _) = foldr1 (\i d -> i <> " " <> d) x

(<+>) :: (Semigroup a, IsString a) => a -> a -> a
l <+> r = l <> " " <> r

punctuate :: (Monoid a) => a -> [a] -> [a]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate a (x : xs) = (x <> a) : punctuate a xs

class (IsString t) => Pretty v t where
  pretty :: v -> Doc t
  prettyList :: [v] -> Doc t
  prettyList = hsep . fmap pretty

instance (IsString t) => Pretty Char t where
  pretty c = text (fromString [c])

instance (IsString t) => Pretty Int t where
  pretty = Text . fromString . show

instance (IsString t, Pretty v t) => Pretty (Maybe v) t where
  pretty (Just v) = pretty v
  pretty Nothing = Empty