{-# LANGUAGE TupleSections #-}
module Utils where
import Data.Maybe (mapMaybe, maybeToList)
import Control.Applicative (Alternative ((<|>), many))



-- | Update the second component of a pair.
--
-- > secondM (\x -> [reverse x, x]) (1,"test") == [(1,"tset"),(1,"test")]
secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f (a,b) = (a,) <$> f b

listSepBy_ :: Alternative m => m [a] -> m [a] -> m [a]
listSepBy_ p sep = listSepBy1_ p sep <|> pure []

listSepBy1_ :: Alternative m => m [a] -> m [a] -> m [a]
listSepBy1_ p sep = (<>) <$> p <*> (
        concat <$> many ((<>) <$> sep <*> p)
    )

-- | @'sepBy_' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ interspersed by values returned by sep.
sepBy_ :: Alternative m => m a -> m (Maybe a) -> m [a]
sepBy_ p sep = sepBy1_ p sep <|> pure []
{-# INLINE sepBy_ #-}

-- | @'sepBy1_' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@ interspersed by values returned by sep.
sepBy1_ :: Alternative m => m a -> m (Maybe a) -> m [a]
sepBy1_ p sep = (:) <$> p <*> (
        concat <$> many ((\x y -> x <> [y]) <$> (maybeToList <$> sep) <*> p)
    )
{-# INLINE sepBy1_ #-}
