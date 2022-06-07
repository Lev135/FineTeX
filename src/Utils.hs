module Utils where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (maybeToList)

(.:) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f .: g = fmap f . g

sequenceSecond :: Functor m => (a, m b) -> m (a, b)
sequenceSecond (a, mb) = (a,) <$> mb

mapSecond :: (Traversable t, Bifunctor p) => (b -> c) -> t (p a b) -> t (p a c)
mapSecond f = fmap (second f)

mapSecondM :: (Monad m, Traversable t) => (b -> m c) -> t (a, b) -> m (t (a, c))
mapSecondM f = mapM sequenceSecond . mapSecond f

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
