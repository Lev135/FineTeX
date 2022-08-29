{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StarIsType #-}

module FineTeX.Processor.Tokenizer
  ( BlackWhiteSet (..),
    BlackWhiteSetList,
    singleton,
    fromConcrete,
    filterSet,
    member,
    intersection,
    Token (..),
    ConflictTokens,
    checkUniqueTokenizing,
    TokenizeMap,
    TokenizeError (..),
    makeTokenizeMap,
    tokenize,
  )
where

import Control.Monad (guard, when)
import Control.Monad.State (State, evalState, gets)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Safe.Exact (splitAtExactMay)

-- * 'BlackWhiteSet' and 'Token' data types and utilities

-- | Set of acceptable ('WhiteSet') or not acceptable ('BlackSet') chars
data BlackWhiteSet c = WhiteSet (Set c) | BlackSet (Set c)
  deriving (Eq, Ord, Show)

-- | Alias for lists of 'BlackWhiteSet's
type BlackWhiteSetList c = [BlackWhiteSet c]

-- | Make 'BlackWhiteSet' accepting only this symbol
singleton :: c -> BlackWhiteSet c
singleton = WhiteSet . S.singleton

-- | Make 'BlackWhiteSetList' accepting only this string
fromConcrete :: [c] -> BlackWhiteSetList c
fromConcrete = map singleton

-- | Filter elements from 'Set' acceptable by the 'BlackWhiteSet'
filterSet :: Ord c => BlackWhiteSet c -> Set c -> Set c
filterSet (WhiteSet ws) = S.intersection ws
filterSet (BlackSet bs) = (`S.difference` bs)

-- | Check if this symbol is acceptable by the 'BlackWhiteSet'
member :: Ord c => c -> BlackWhiteSet c -> Bool
member c (WhiteSet ws) = S.member c ws
member c (BlackSet bs) = S.notMember c bs

-- | Intersection of two 'BlackWhiteSet's.
-- Accepts only those symbols, that are acceptable by both arguments
intersection :: Ord c => BlackWhiteSet c -> BlackWhiteSet c -> BlackWhiteSet c
intersection (WhiteSet ws) (WhiteSet ws') = WhiteSet $ S.intersection ws ws'
intersection (WhiteSet ws) (BlackSet bs') = WhiteSet $ S.difference ws bs'
intersection (BlackSet bs) (WhiteSet ws') = WhiteSet $ S.difference ws' bs
intersection (BlackSet bs) (BlackSet bs') = BlackSet $ S.union bs bs'

-- | 'k' --- type of token's name
-- 'c' --- type of symbols to be tokenized
-- 'r' --- type of result of tokenizing (if tokenizing succeeds,
-- 'postProc' function is called on **body** to achieve result)
--
-- Example:
--
-- @
--   Token {
--      body   = [fromList ['a', 'b'], fromList ['c']],
--      behind = [WhiteSet $ fromList ['d'], BlackSet $ fromList ['e', 'f']],
--      ahead  = [BlackSet $ fromList ['g']],
--      postProc = id
--   }
-- @
-- accepts strings "ac" and "bc" and also parts of strings
-- "d2ac3" ("ac"), "0d2bc" ("bc"), "2bc345" ("bc")
-- where numbers indicate arbitrary symbols, @2 /= e@, @2 /= f@, @3 /= g@
data Token k c r = Token
  { -- | Name of token. Used in error messages
    name :: k,
    body :: [Set c],
    behind, ahead :: BlackWhiteSetList c,
    -- | Function to be applied after tokenizing to achieve result \\
    -- **NB! Only 'body' symbols are given as argument to this function**
    postProc :: [c] -> r
  }

-- * Checking tokenize uniqueness

type TokId = Int

data RToken c = RToken
  { tokId :: TokId,
    body :: [Set c],
    rbehind, ahead :: [BlackWhiteSet c]
  }
  deriving (Show)

instance Eq (RToken c) where
  (==) = (==) `on` (\RToken {tokId} -> tokId)

instance Ord (RToken c) where
  compare = compare `on` (\RToken {tokId} -> tokId)

data Suff c = Suff
  { rbehind :: [Set c],
    body :: [Set c],
    ahead :: [BlackWhiteSet c]
  }
  deriving (Eq, Ord, Show)

data PatDiv c = PatDiv
  { rprefPatIds, rpatIds :: [TokId],
    suff :: Suff c
  }
  deriving (Eq, Ord, Show)

zipWithKeepRest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithKeepRest _ [] ys = ys
zipWithKeepRest _ xs [] = xs
zipWithKeepRest g (x : xs) (y : ys) = g x y : zipWithKeepRest g xs ys

initDiv :: RToken cs -> PatDiv cs
initDiv RToken {tokId, body, ahead} =
  PatDiv
    { rprefPatIds = [],
      rpatIds = [tokId],
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
divStep :: Ord c => Int -> PatDiv c -> RToken c -> Maybe (PatDiv c)
divStep
  maxBehind
  PatDiv {rprefPatIds, rpatIds, suff = Suff {rbehind, body, ahead}}
  RToken {tokId, body = pbody, rbehind = prbehind, ahead = pahead} =
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
      let (rprefPatIds', rpatIds') =
            if length pbody <= length body
              then (tokId : rprefPatIds, rpatIds)
              else (rprefPatIds, tokId : rpatIds)
      return
        PatDiv
          { rprefPatIds = rprefPatIds',
            rpatIds = rpatIds',
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

makeRToken :: TokId -> Token k c r -> RToken c
makeRToken tokId Token {body, behind, ahead} =
  RToken
    { tokId,
      body = body,
      rbehind = reverse behind,
      ahead = ahead
    }

-- | Check if every list composed from the set of tokens can be uniquely decomposed into tokens
checkUniqueTokenizing :: forall k c r. (Ord c) => [(k, Token k c r)] -> Either (ConflictTokens k) ()
checkUniqueTokenizing pats =
  first (bimap reverse reverse) $
    mapM_ (h S.empty) [res | p <- rpats, p' <- rpats, p /= p', res <- maybeToList $ divStep maxBehind (initDiv p') p]
  where
    rpats = zipWith makeRToken [0 ..] (snd <$> pats)
    getTokName tokId = fst $ pats !! tokId
    maxBehind = maximum $ (\RToken {rbehind} -> length rbehind) <$> rpats
    h :: Set (Suff c) -> PatDiv c -> Either (ConflictTokens k) ()
    h olds pdiv@PatDiv {rprefPatIds, rpatIds, suff = suff@Suff {body}} = do
      when (null body) $
        Left (getTokName <$> rprefPatIds, getTokName <$> rpatIds)
      mapM_ (h (S.insert suff olds)) $
        filter ((`S.notMember` olds) . (\PatDiv {suff} -> suff)) $
          catMaybes $ divStep maxBehind pdiv <$> rpats

-- * Tokenizing

data RToken' c = RToken'
  { tokId :: TokId,
    body :: [Set c],
    rbehind, ahead :: [BlackWhiteSet c]
  }
  deriving (Eq, Ord, Show)

modifyIds :: (TokId -> TokId) -> RToken' c -> RToken' c
modifyIds f tok@RToken' {tokId} = tok {tokId = f tokId}

-- | Map with data for 'tokenize' function. Should be prepared by calling
-- 'makeTokenizeMap' function
--
-- 'Semigroup' instance can be useful for inserting new elements
data TokenizeMap k c r = TokenizeMap
  { tokMap :: Map c [RToken' c],
    procFuncs :: IntMap ([c] -> r),
    tokNames :: IntMap k
  }

instance (Show k, Show c, Show r) => Show (TokenizeMap k c r) where
  show TokenizeMap {tokMap, tokNames} =
    unlines
      [ "tokMap = " <> show tokMap,
        "tokNames = " <> show tokNames
      ]

instance Ord c => Semigroup (TokenizeMap k c r) where
  TokenizeMap tokMap' procFuncs' tokNames'
    <> TokenizeMap tokMap'' procFuncs'' tokNames'' =
      TokenizeMap
        { tokMap = M.unionWith (<>) tokMap' tokMap''',
          procFuncs = procFuncs' <> procFuncs''',
          tokNames = tokNames' <> tokNames'''
        }
      where
        len = length tokMap'
        tokMap''' = map (modifyIds (+ len)) <$> tokMap''
        procFuncs''' = IM.mapKeysMonotonic (+ len) procFuncs''
        tokNames''' = IM.mapKeysMonotonic (+ len) tokNames''

instance Ord c => Monoid (TokenizeMap k c r) where
  mempty = TokenizeMap mempty mempty mempty

singleTokMap :: Eq c => Token k c r -> TokenizeMap k c r
singleTokMap Token {name, body, behind, ahead, postProc} =
  TokenizeMap
    { tokMap = M.fromAscList $ map (,[rtok]) $ S.toList $ head body,
      procFuncs = IM.singleton tokId postProc,
      tokNames = IM.singleton tokId name
    }
  where
    tokId = 0
    rtok = RToken' {tokId, body, rbehind = reverse behind, ahead}

-- | Insert 'Token' into 'TokenizeMap'
insert :: Ord c => Token k c r -> TokenizeMap k c r -> TokenizeMap k c r
insert tok = (<> singleTokMap tok)

-- | Create auxillary Map for tokenizing. Should be called once for initializing
makeTokenizeMap :: Ord c => [Token k c r] -> TokenizeMap k c r
makeTokenizeMap = foldr insert mempty

-- | Error during tokenizing
data TokenizeError k c
  = NoWayTokenize
      Int
      -- ^ Position of the first character that can not be tokenized
      [(k, [c])]
      -- ^ Part of string successfully tokenized (the longest of all attempts)
  | TwoWaysTokenize
      Int
      -- ^ Length of uniquely tokenized prefix
      [(k, [c])]
      -- ^ First tokenize way
      [(k, [c])]
      -- ^ Second tokenize way
  deriving (Show, Eq)

mapTokErrKey :: (k -> k') -> TokenizeError k c -> TokenizeError k' c
mapTokErrKey f (NoWayTokenize pos toks) =
  NoWayTokenize pos (map (first f) toks)
mapTokErrKey f (TwoWaysTokenize pos toks toks') =
  TwoWaysTokenize pos (map (first f) toks) (map (first f) toks')

-- | Split list of symbols on tokens.
tokenize :: forall k c r. Ord c => TokenizeMap k c r -> [c] -> Either (TokenizeError k c) [r]
tokenize TokenizeMap {tokMap, tokNames, procFuncs} cs =
  bimap (mapTokErrKey (tokNames IM.!)) (map (uncurry (procFuncs IM.!))) $
    flip evalState mempty $ h 0 [] cs
  where
    -- input string is split in two parts: (reversed) @prevs@ and @nexts@
    -- @pos == length prevs@
    -- prevs are assumed to be already processed
    -- returns unique possible first token's result at the @pos@ position
    h :: Int -> [c] -> [c] -> State (IntMap (Res c)) (Res c)
    h _ _ [] = pure $ Right []
    h pos prevs nexts@(cur : _) = do
      -- get memorized result
      mres <- gets $ IM.lookup pos
      maybe acceptedToks pure mres
      where
        allToks :: [RToken' c]
        allToks = fromMaybe [] $ M.lookup cur tokMap

        acceptedToks :: State (IntMap (Res c)) (Res c)
        acceptedToks =
          foldrM
            ( \(tokId, len, curs, nexts') res'' -> do
                let curTok = (tokId, curs)
                res' <- addTok curTok <$> h (pos + len) (reverse curs <> prevs) nexts'
                pure $ case (res', res'') of
                  (Left TwoWaysTokenize {}, _) -> res'
                  (_, Left TwoWaysTokenize {}) -> res''
                  (Left NoWayTokenize {}, Right _) -> res''
                  (Right _, Left NoWayTokenize {}) -> res'
                  (Left (NoWayTokenize l' _), Left (NoWayTokenize l'' _)) ->
                    if l' > l'' then res' else res''
                  (Right toks', Right toks'') ->
                    Left $ TwoWaysTokenize pos toks' toks''
            )
            ((Left $ NoWayTokenize pos []) :: Res c)
            $ mapMaybe (accepts prevs nexts) allToks
        addTok :: (TokId, [c]) -> Res c -> Res c
        addTok tok = \case
          Left (NoWayTokenize pos toks) ->
            Left $ NoWayTokenize pos (tok : toks)
          Left (TwoWaysTokenize len toks toks') ->
            Left $ TwoWaysTokenize len (tok : toks) (tok : toks')
          Right rs -> Right $ tok : rs

        accepts :: [c] -> [c] -> RToken' c -> Maybe (TokId, Int, [c], [c])
        accepts rprevs nexts RToken' {tokId, rbehind, body, ahead} = do
          let len = length body
          (curs, nexts') <- splitAtExactMay len nexts
          mapM_ guard $ zipWith member rprevs rbehind
          mapM_ guard $ zipWith S.member curs body
          mapM_ guard $ zipWith member nexts' ahead
          return (tokId, len, curs, nexts')

type Res c = Either (TokenizeError TokId c) [(TokId, [c])]
