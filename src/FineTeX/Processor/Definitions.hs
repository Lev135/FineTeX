{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FineTeX.Processor.Definitions
  ( State (..),
    initState,
    ProcessDefsError,
    processDefs,
  )
where

import Control.Lens (At (at), Field1 (_1), Lens', anon, non, to, use, (%=), (<>=), (^.))
import Control.Monad (forM_, when)
import Control.Monad.RWS (MonadState, MonadWriter (tell))
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax
import FineTeX.Processor.Syntax
import FineTeX.Processor.Tokenizer (BlackWhiteSet (..), BlackWhiteSetList)
import qualified FineTeX.Processor.Tokenizer as Tok
import FineTeX.Utils (Box (unBox), Pos, PosC (..), PrettyErr (..), prettyPos, sequenceBox)
import qualified Prettyprinter as P
import Safe.Exact (splitAtExactNote)

-- Processing definitions

data ProcessDefsError
  = MultipleDecl DefType Text Pos Pos
  | UndefinedMode Text Pos
  | UndefinedSort Text Pos
  deriving (Show)

instance PrettyErr ProcessDefsError where
  prettyErr src (MultipleDecl t name p p') =
    P.vsep
      [prettyPos src lbl p, prettyPos src "Previously defined here" p']
    where
      lbl = "Multiple definitions of " <> tName <> " '" <> name <> "'"
      tName = case t of
        DefTEnv -> "environment"
        DefTCmd -> "command"
        DefTInl -> "inline"
        DefTMode -> "mode"
        DefTSort -> "sort"
        DefTPref -> "pref"
  prettyErr src (UndefinedMode name p) = prettyPos src lbl p
    where
      lbl = "Undefined mode '" <> name <> "'"
  prettyErr src (UndefinedSort name p) = prettyPos src lbl p
    where
      lbl =
        T.unlines
          [ "Undefined sort '" <> name <> "'",
            "Note: unlike other definitions, sort must be defined before used"
          ]

data State p = State
  { _definitions :: Definitions p,
    _tokens :: [Token]
  }

initState :: Definitions p -> State p
initState _definitions = State {_definitions, _tokens = []}

type MState m p =
  (MonadWriter [ProcessDefsError] m, MonadState (State p) m)

data DefType = DefTEnv | DefTPref | DefTCmd | DefTInl | DefTMode | DefTSort
  deriving (Show)

-- | Processing definitions.
--   Initial state should contain definitions, imported from other files
processDefs ::
  forall m p.
  (PosC p, MState m p) =>
  DefBlock p ->
  m ()
processDefs defs = do
  -- Lay out definitions in appropriate maps and check conflicting names
  procDefBlock defs
  inModeModes <- use $ definitions . inModes . to (map (second getPos) . M.toList)
  -- Check modes for which we found definitions to be defined
  forM_ inModeModes $ \(pname, p) -> do
    mmode <- use $ definitions . modes . at pname
    when (isNothing mmode) $ tell [UndefinedMode pname p]

-- TODO: make check for uniquely parsing of commands (rename ~~> tokens? prims?)

procDefBlock :: forall m p. (PosC p, MState m p) => DefBlock p -> m ()
procDefBlock = mapM_ $ \case
  DefModeBlock defs -> mapM_ (addDef modes DefTMode) defs
  DefSortBlock defs -> mapM_ procSort defs
  DefInModeBlock mode defs -> do
    mapM_ (procDefInModeBlock mode) defs
    toks <- use tokens
    let x = Tok.makeTokenizeMap toks
        h g pa = sequenceBox $ g <$> pa
        lens = definitions . inModes . at (unBox mode) . anon (mempty <$ mode) null . h
    lens . tokMap %= (<> x)

instance HasName (DefInline p) (p Text) where
  name = borders . _1

procDefInModeBlock ::
  forall m p. (PosC p, MState m p) => p Text -> DefInModeBlock p -> m ()
procDefInModeBlock mode = \case
  DefEnvBlock defs -> mapM_ (addInMode envs DefTEnv) defs
  DefRuleBlock defs -> mapM_ (procRuleDef mode) defs
  DefPrefBlock defs -> mapM_ (addInMode prefs DefTPref) defs
  DefInlBlock defs -> mapM_ (addInMode inlines DefTInl) defs
  where
    addInMode ::
      HasName (a p) (p Text) =>
      Lens' (InModeDefs p) (Map Text (a p)) ->
      DefType ->
      (a p -> m ())
    addInMode lens deft = addDef (inModes . at (unBox mode) . anon (mempty <$ mode) null . h . lens) deft
    h g pa = sequenceBox $ g <$> pa

procSort :: DefSort p -> m ()
procSort = undefined

lens modeName = definitions . inModes . at (unBox modeName) . anon (mempty <$ modeName) null . h
  where
    h g pa = sequenceBox $ g <$> pa

procRuleDef :: forall m p. (PosC p, MState m p) => p ModeName -> DefRule p -> m ()
procRuleDef modeName DefRule {_match, _rule} = do
  i <- use $ lens modeName . rules . to M.keysSet . to S.lookupMax . non 0
  lens modeName . rules %= M.insert i _rule
  tokAlts <- pmExpToTokenAlts i _match
  tokens <>= tokAlts -- (<> ((i,) <$> tokAlts))

pmExpToTokenAlts :: forall m p. (PosC p, MState m p) => Id -> PatMatchExp p -> m [Token]
pmExpToTokenAlts i PatMatchExp {_behind, _current, _ahead} = do
  behindBWs <- expToBWSetListAlts _behind
  aheadBWs <- expToBWSetListAlts _ahead
  currentSets <-
    foldr h [(const mempty, [])]
      <$> mapM (\PatMatchEl {_var, _sort} -> (_var,) <$> expToBWSetListAlts _sort) _current
  pure
    [ Tok.Token {body = toSet <$> cur, behind, ahead, name = i, postProc = (i,) . f}
      | (f, cur) <- currentSets,
        behind <- behindBWs,
        ahead <- aheadBWs
    ]
  where
    h ::
      (Maybe (p VarName), [BlackWhiteSetList Char]) ->
      [(String -> Map VarName String, BlackWhiteSetList Char)] ->
      [(String -> Map VarName String, BlackWhiteSetList Char)]
    h (curVar, curList) xs = [(makef' f s', s <> s') | (f, s) <- xs, s' <- curList]
      where
        makef' ::
          (String -> Map VarName String) ->
          BlackWhiteSetList Char ->
          String ->
          Map VarName String
        makef' f curBWSetList str =
          let (curStr, str') = splitAtExactNote "Too small str" (length curBWSetList) str
           in case unBox <$> curVar of
                Nothing -> f str'
                Just varName -> M.insert varName curStr (f str')

toSet :: BlackWhiteSet c -> S.Set c
toSet (WhiteSet s) = s
toSet (BlackSet _) = error "Black set" -- TODO: fix it

-- | Make a 'BlackWhiteSetList's equivalent (as a disjunction) to 'SortExp'
expToBWSetListAlts :: forall m p. (PosC p, MState m p) => SortExp p -> m [BlackWhiteSetList Char]
expToBWSetListAlts = \case
  SEString str -> pure [Tok.fromConcrete (T.unpack $ unBox str)]
  SESpace -> pure [[Tok.singleton ' ']]
  SESort sortName -> do
    mz <- use $ definitions . sorts . at (unBox sortName)
    case mz of
      Nothing -> do
        tell [UndefinedSort (unBox sortName) (getPos sortName)]
        pure []
      Just toks -> pure $ unBox toks
  SEConcat e e' -> do
    bws <- expToBWSetListAlts e
    bws' <- expToBWSetListAlts e'
    pure [bw <> bw' | bw <- bws, bw' <- bws']
  SEOr e e' -> do
    bws <- expToBWSetListAlts e
    bws' <- expToBWSetListAlts e'
    pure $ bws <> bws'
  SEVoid -> pure []
  SEEmpty -> pure [[]]

addDef ::
  forall m p a.
  (PosC p, MState m p, HasName (a p) (p Text)) =>
  Lens' (Definitions p) (Map Text (a p)) ->
  DefType ->
  a p ->
  m ()
addDef lens deft val = do
  mval' <- use $ definitions . lens . at (val ^. name . to unBox)
  case mval' of
    Nothing -> definitions . lens %= M.insert (val ^. name . to unBox) val
    Just val' ->
      tell [MultipleDecl deft (unBox $ val ^. name) (getPos $ val ^. name) (getPos $ val' ^. name)]

instance HasDefinitions (State p_a1m9hb) (Definitions p_a1m9hb) where
  {-# INLINE definitions #-}
  definitions f_a1ma8U (State x1_a1ma8V x3_a1ma8X) =
    fmap
      (`State` x3_a1ma8X)
      (f_a1ma8U x1_a1ma8V)

class HasTokens s a | s -> a where
  tokens :: Lens' s a

instance HasTokens (State p_a1m9hb) [Token] where
  {-# INLINE tokens #-}
  tokens f_a1ma96 (State x1_a1ma97 x3_a1ma99) =
    fmap
      (State x1_a1ma97)
      (f_a1ma96 x3_a1ma99)
