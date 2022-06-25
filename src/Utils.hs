{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import Text.Megaparsec (SourcePos (..), unPos)

(.:) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f .: g = fmap f . g

-- | Convert a 'Maybe' value to a value in any monad
failMsg :: MonadFail m => Maybe a -> String -> m a
failMsg Nothing err = fail err
failMsg (Just x) _ = return x

eitherFail :: MonadFail m => Either String a -> m a
eitherFail (Left e) = fail e
eitherFail (Right a) = return a

infix 4 `failMsg`

-- | Update the second component of a pair.
--
-- > secondM (\x -> [reverse x, x]) (1,"test") == [(1,"tset"),(1,"test")]
secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f (a, b) = (a,) <$> f b

listSepBy_ :: Alternative m => m [a] -> m [a] -> m [a]
listSepBy_ p sep = listSepBy1_ p sep <|> pure []

listSepBy1_ :: Alternative m => m [a] -> m [a] -> m [a]
listSepBy1_ p sep =
  (<>) <$> p
    <*> ( concat <$> many ((<>) <$> sep <*> p)
        )

-- | @'sepBy_' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ interspersed by values returned by sep.
sepBy_ :: Alternative m => m a -> m (Maybe a) -> m [a]
sepBy_ p sep = sepBy1_ p sep <|> pure []
{-# INLINE sepBy_ #-}

-- | @'sepBy1_' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ interspersed by values returned by sep.
sepBy1_ :: Alternative m => m a -> m (Maybe a) -> m [a]
sepBy1_ p sep =
  (:) <$> p
    <*> ( concat <$> many ((\x y -> x <> [y]) <$> (maybeToList <$> sep) <*> p)
        )
{-# INLINE sepBy1_ #-}

withError :: MonadError e m => (e -> e) -> m a -> m a
withError f ma = catchError ma (throwError . f)

type Pos = (SourcePos, SourcePos)

class
  ( Traversable p,
    (forall a. Eq a => Eq (p a)),
    (forall a. Ord a => Ord (p a)),
    (forall a. Show a => Show (p a))
  ) =>
  Box p
  where
  unBox :: p a -> a

foldMapBox :: Box p => (a -> b) -> p a -> b
foldMapBox f pa = unBox $ f <$> pa

sequenceBox :: (Functor f, Box p) => p (f a) -> f (p a)
sequenceBox pma = (<$ pma) <$> unBox pma

class Box p => PosC p where
  getPos :: p a -> Pos

type SourceText = [Text]

class PrettyErr e where
  prettyErr :: SourceText -> e -> ErrDoc

class PrettyErrType et where
  prettyErrType :: et -> Text

data Error et e' = SimpleErr et Pos | ComplexErr e'

instance PrettyErr Void where
  prettyErr _ = absurd

instance (PrettyErrType et, PrettyErr e') => PrettyErr (Error et e') where
  prettyErr src (SimpleErr et p) = prettyPos src (prettyErrType et) p
  prettyErr src (ComplexErr e') = prettyErr src e'

type ErrDoc = P.Doc Void

prettyPos :: SourceText -> Text -> Pos -> ErrDoc
prettyPos lines msg (b, e) =
  P.vcat
    [ P.hcat (P.pretty <$> [file, ":", show line, ":", show col, ": "])
        <> P.pretty msg,
      P.pretty $ lines !! (line - 1),
      P.pretty mask
    ]
  where
    file = sourceName b
    line = unPos $ sourceLine b
    col = unPos $ sourceColumn b
    col' = unPos $ sourceColumn e
    mask =
      T.concat
        [ repl (col - 1) ' ',
          T.singleton '^',
          repl (col' - col - 1) '~',
          T.singleton '^'
        ]
    repl n ch = T.replicate n (T.singleton ch)

renderErrors :: PrettyErr e => Text -> [e] -> Text
renderErrors file =
  P.renderStrict . P.layoutSmart errRenderOpts . P.vcat . map (prettyErr (T.lines file))
  where
    errRenderOpts = P.defaultLayoutOptions {P.layoutPageWidth = P.Unbounded}
