{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module FineTeX.Utils where

import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.RWS (MonadState (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import Text.Megaparsec (SourcePos (..), unPos)

(.:) :: (b -> c) -> (a -> a' -> b) -> a -> a' -> c
f .: g = fmap f . g

-- | Convert a 'Maybe' value to a value in any monad
failMsg :: MonadFail m => Maybe a -> String -> m a
failMsg Nothing err = fail err
failMsg (Just x) _ = return x

infix 4 `failMsg`

-- | Like 'span' but with 'Maybe' predicate
-- *(from Distribution.Utils.Generic)*
spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ xs@[] = ([], xs)
spanMaybe p xs@(x : xs') = case p x of
  Just y -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
  Nothing -> ([], xs)

withError :: MonadError e m => (e -> e) -> m a -> m a
withError f ma = catchError ma (throwError . f)

localState :: MonadState s m => m a -> m a
localState ma = do
  state <- get
  a <- ma
  put state
  return a

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

type Wrap p x = p (x p)

foldMapBox :: Box p => (a -> b) -> p a -> b
foldMapBox f pa = unBox $ f <$> pa

sequenceBox :: (Functor f, Box p) => p (f a) -> f (p a)
sequenceBox pma = (<$ pma) <$> unBox pma

class Box p => PosC p where
  getPos :: p a -> Pos

type Sources = Map FilePath [Text]

class PrettyErr e where
  prettyErr :: Sources -> e -> ErrDoc

type ErrDoc = P.Doc Void

prettyPos :: Sources -> Text -> Pos -> ErrDoc
prettyPos srcs msg (b, e) =
  P.vcat
    [ P.hcat (P.pretty <$> [file, ":", show line, ":", show col, ": "])
        <> P.pretty msg,
      code
    ]
  where
    file = sourceName b
    line = unPos $ sourceLine b
    col = unPos $ sourceColumn b
    code = case M.lookup file srcs of
      Nothing -> P.pretty ("Unable to show source lines" :: Text)
      Just lines ->
        let lineStr = lines !! (line - 1)
            col'
              | unPos (sourceLine e) == line = unPos $ sourceColumn e
              | otherwise = T.length lineStr
            mask =
              T.concat
                [ repl (col - 1) ' ',
                  T.singleton '^',
                  repl (col' - col - 2) '~',
                  if col' == col + 1 then T.empty else T.singleton '^'
                ]
            repl n ch = T.replicate n (T.singleton ch)
         in P.vcat [P.pretty lineStr, P.pretty mask]

renderErrors :: PrettyErr e => Sources -> [e] -> Text
renderErrors srcs =
  P.renderStrict . P.layoutSmart errRenderOpts . P.vcat . map (prettyErr srcs)
  where
    errRenderOpts = P.defaultLayoutOptions {P.layoutPageWidth = P.Unbounded}
