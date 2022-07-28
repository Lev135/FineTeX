module FineTeX.Processor.Tokenizer
  ( BlackWhiteSet (..),
    Token (..),
    ConflictTokens,
    checkUniqueTokenizing,
    TokenizeMap,
    makeTokenizeMap,
    tokenize,
    revToken,
    isAcceptable,
  )
where

import Control.Monad (guard, when)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S

-- | Set of acceptable ('WhiteSet') or not acceptable ('BlackSet') chars
data BlackWhiteSet c = WhiteSet (Set c) | BlackSet (Set c)
  deriving (Eq, Ord, Show)

filterSet :: Ord c => BlackWhiteSet c -> Set c -> Set c
filterSet (WhiteSet ws) = S.intersection ws
filterSet (BlackSet bs) = (`S.difference` bs)

member :: Ord c => c -> BlackWhiteSet c -> Bool
member c (WhiteSet ws) = S.member c ws
member c (BlackSet bs) = S.notMember c bs

intersection :: Ord c => BlackWhiteSet c -> BlackWhiteSet c -> BlackWhiteSet c
intersection (WhiteSet ws) (WhiteSet ws') = WhiteSet $ S.intersection ws ws'
intersection (WhiteSet ws) (BlackSet bs') = WhiteSet $ S.difference ws bs'
intersection (BlackSet bs) (WhiteSet ws') = WhiteSet $ S.difference ws' bs
intersection (BlackSet bs) (BlackSet bs') = WhiteSet $ S.union bs bs'

-- |
-- Example: Token {
--    name   = "EXAMPLE"
--    body   = [fromList ['a', 'b'], fromList ['c']],
--    behind = [WhiteSet $ fromList ['d'], BlackSet $ fromList ['e', 'f']],
--    ahead  = [BlackSet $ fromList ['g']]
-- }
-- accepts strings "ab" and "ac" and also parts of strings
-- "d2ac3" ("ac"), "0d2bc" ("bc"), "2bc345" ("bc")
-- where numbers indicate arbitrary symbols, @2 /= e@, @2 /= f@, @3 /= g@
data Token k c = Token
  { -- | unique name of token
    name :: k,
    -- | list of sets of acceptable elements on each position
    body :: [Set c],
    -- | list of sets of NOT acceptable elements on each position before/after token
    behind, ahead :: [BlackWhiteSet c]
  }
  deriving (Eq, Ord, Show)

data RToken k c = RToken
  { name :: k,
    body :: [Set c],
    rbehind, ahead :: [BlackWhiteSet c]
  }
  deriving (Eq, Ord, Show)

data Suff c = Suff
  { rbehind :: [Set c],
    body :: [Set c],
    ahead :: [BlackWhiteSet c]
  }
  deriving (Eq, Ord, Show)

data PatDiv k c = PatDiv
  { rprefPatNames, rpatNames :: [k],
    suff :: Suff c
  }
  deriving (Eq, Ord, Show)

zipWithKeepRest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithKeepRest _ [] ys = ys
zipWithKeepRest _ xs [] = xs
zipWithKeepRest g (x : xs) (y : ys) = g x y : zipWithKeepRest g xs ys

initDiv :: RToken k cs -> PatDiv k cs
initDiv RToken {name, body, ahead} =
  PatDiv
    { rprefPatNames = [],
      rpatNames = [name],
      suff =
        Suff
          { rbehind = [],
            body,
            ahead
          }
    }

{-
  behind           big body            big ahead
  --------|=========================|~~~~~~~~~~~~~~
  --------|============|~~~~~~~~~~~~~~~~~~~~~~~~~~~
  behind    small body         small ahead

  -----------------------|============|~~~~~~~~~~~~~
       res behind          res body      res ahead
-}
divStep :: Ord c => Int -> PatDiv k c -> RToken k c -> Maybe (PatDiv k c)
divStep
  maxBehind
  PatDiv {rprefPatNames, rpatNames, suff = Suff {rbehind, body, ahead}}
  RToken {name, body = pbody, rbehind = prbehind, ahead = pahead} =
    do
      let rbehind' = filterPref prbehind rbehind
      guard $ not $ any null rbehind'
      let drbehind = zipWith S.intersection pbody body
      guard $ not $ any null drbehind
      let (bigBody, smallBody, bigAhead, smallAhead) =
            if length pbody <= length body
              then (body, pbody, ahead, pahead)
              else (pbody, body, pahead, ahead)
      let body' = filterPref smallAhead $ drop (length smallBody) bigBody
      guard $ not $ any null body'
      let ahead' =
            zipWithKeepRest
              intersection
              bigAhead
              (drop (length bigBody - length smallBody) smallAhead)
      let (rprefPatNames', rpatNames') =
            if length pbody <= length body
              then (name : rprefPatNames, rpatNames)
              else (rprefPatNames, name : rpatNames)
      return
        PatDiv
          { rprefPatNames = rprefPatNames',
            rpatNames = rpatNames',
            suff =
              Suff
                { rbehind = take maxBehind $ reverse drbehind <> rbehind',
                  body = body',
                  ahead = ahead'
                }
          }
    where
      filterPref [] ys = ys
      filterPref _ [] = error "Long prefix"
      filterPref (x : xs) (y : ys) = filterSet x y : filterPref xs ys

-- | Two sequences of tokens, that lead to the same list
type ConflictTokens k = ([k], [k])

revToken :: Token k c -> RToken k c
revToken Token {name, body, behind, ahead} =
  RToken {name, body, rbehind = reverse behind, ahead}

-- | Check if every list composed from the set of tokens can be uniquely decomposed into tokens
checkUniqueTokenizing :: forall k c. (Eq k, Ord c) => [Token k c] -> Either (ConflictTokens k) ()
checkUniqueTokenizing pats =
  first (bimap reverse reverse) $
    mapM_ (h S.empty) [res | p <- rpats, p' <- rpats, p /= p', res <- maybeToList $ divStep maxBehind (initDiv p') p]
  where
    rpats = revToken <$> pats
    maxBehind = maximum $ (\RToken {rbehind} -> length rbehind) <$> rpats
    h :: Set (Suff c) -> PatDiv k c -> Either (ConflictTokens k) ()
    h olds pdiv@PatDiv {rprefPatNames, rpatNames, suff = suff@Suff {body}} = do
      when (null body) $
        Left (rprefPatNames, rpatNames)
      mapM_ (h (S.insert suff olds)) $
        filter ((`S.notMember` olds) . (\PatDiv {suff} -> suff)) $
          catMaybes $ divStep maxBehind pdiv <$> rpats

-- | Map with data for 'tokenize' function. Should be prepared by calling
-- 'makeTokenizeMap' function
type TokenizeMap k c = Map c [RToken k c]

-- | Create auxillary Map for tokenizing. Should be called once for initializing
makeTokenizeMap :: forall k c. Ord c => [Token k c] -> TokenizeMap k c
makeTokenizeMap = M.unionsWith (<>) . map h
  where
    h :: Token k c -> Map c [RToken k c]
    h p@Token {body} = M.fromAscList $ map (,[revToken p]) $ S.toList $ head body

-- | Split list of symbols on tokens.
-- Error value '[[k]]' indicates tokenizer's ways on splitting list
-- (can be used to produce error messages)
tokenize :: forall k c. Ord c => TokenizeMap k c -> [c] -> Either [[k]] [k]
tokenize tokMap = h []
  where
    h :: [c] -> [c] -> Either [[k]] [k]
    h _ [] = Right []
    h prevs curs@(c : _) = do
      pats <- case M.lookup c tokMap of
        Nothing -> Left []
        Just pats -> Right pats
      foldr h'' (Left [[]]) $ h' <$> pats
      where
        h' :: RToken k c -> Either [[k]] [k]
        h' rtok@RToken {name, body}
          | isAcceptable rtok prevs curs =
            bimap ((name :) <$>) (name :) $
              h (prevs <> dprevs) curs'
          | otherwise = Left []
          where
            (dprevs, curs') = splitAt (length body) curs

        h'' :: Either [[k]] [k] -> Either [[k]] [k] -> Either [[k]] [k]
        h'' (Right v) _ = Right v
        h'' _ (Right v) = Right v
        h'' (Left e) (Left e') = Left (e <> e')

isAcceptable :: Ord c => RToken k c -> [c] -> [c] -> Bool
isAcceptable RToken {rbehind, body, ahead} prevs curs =
  and
    ( [ length body <= length curs,
        and $ zipWith member prevs rbehind,
        and $ zipWith S.member curs body,
        and $ zipWith member (drop (length body) curs) ahead
      ] ::
        [Bool]
    )
