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
import Control.Monad (forM_, unless, when)
import Control.Monad.RWS (MonadState, MonadWriter (tell))
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils (Posed, getPos, getVal, sequencePosed)
import FineTeX.Processor.Syntax
import FineTeX.Processor.Tokenizer (BlackWhiteSet (..), BlackWhiteSetList)
import qualified FineTeX.Processor.Tokenizer as Tok
import FineTeX.Utils (Pos, PrettyErr (..), prettyPos)
import qualified Prettyprinter as P
import Safe.Exact (splitAtExactNote)

-- Processing definitions

data ProcessDefsError
  = MultipleDecl DefType Text Pos Pos
  | UndefinedMode Text Pos
  | UndefinedSort Text Pos
  | UnknownIdentifier Text Pos
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
  prettyErr src (UnknownIdentifier name p) = prettyPos src lbl p
    where
      lbl = "Unknown identifier '" <> name <> "'"

data State = State
  { _definitions :: Definitions,
    _tokens :: [Token]
  }

initState :: Definitions -> State
initState _definitions = State {_definitions, _tokens = []}

type MState m = (MonadWriter [ProcessDefsError] m, MonadState State m)

data DefType = DefTEnv | DefTPref | DefTCmd | DefTInl | DefTMode | DefTSort
  deriving (Show)

-- | Processing definitions.
--   Initial state should contain definitions, imported from other files
processDefs :: forall m. MState m => DefBlock -> m ()
processDefs defs = do
  -- Lay out definitions in appropriate maps and check conflicting names
  procDefBlock defs
  inModeModes <- use $ definitions . inModes . to (map (second getPos) . M.toList)
  -- Check modes for which we found definitions to be defined
  forM_ inModeModes $ \(pname, p) -> do
    mmode <- use $ definitions . modes . at pname
    when (isNothing mmode) $ tell [UndefinedMode pname p]

-- TODO: make check for uniquely parsing of commands (rename ~~> tokens? prims?)

procDefBlock :: forall m. MState m => DefBlock -> m ()
procDefBlock = mapM_ $ \case
  DefModeBlock defs -> mapM_ (addDef modes DefTMode) defs
  DefSortBlock defs -> mapM_ procSort defs
  DefInModeBlock mode defs -> do
    mapM_ (procDefInModeBlock mode) defs
    toks <- use tokens
    let x = Tok.makeTokenizeMap toks
    lens mode . tokMap <>= x

instance HasName DefInline (Posed Text) where
  name = borders . _1

procDefInModeBlock :: forall m. MState m => Posed Text -> DefInModeBlock -> m ()
procDefInModeBlock mode = \case
  DefEnvBlock defs ->
    mapM_
      ( \def -> do
          mapM_
            (checkRuleTerm (def ^. args))
            ((def ^. begin) <> (def ^. end))
          addInMode envs DefTEnv def
      )
      defs
  DefRuleBlock defs -> mapM_ (procRuleDef mode) defs
  DefPrefBlock defs ->
    mapM_
      ( \def -> do
          mapM_
            (checkRuleTerm (def ^. args))
            ((def ^. pref) <> (def ^. suf) <> (def ^. sep))
          mapM_
            (checkRuleTerm [])
            ((def ^. begin) <> (def ^. end))
          checkMode (def ^. innerModeName)
          addInMode prefs DefTPref def
      )
      defs
  DefInlBlock defs -> mapM_ (addInMode inlines DefTInl) defs
  where
    addInMode ::
      HasName a (Posed Text) =>
      Lens' InModeDefs (Map Text a) ->
      DefType ->
      (a -> m ())
    addInMode lens deft = addDef (inModes . at (getVal mode) . anon (mempty <$ mode) null . h . lens) deft
    h g pa = sequencePosed $ g <$> pa

checkRuleTerm :: MState m => [Argument] -> RuleTerm -> m ()
checkRuleTerm args = \case
  RTString _ -> pure ()
  RTVar varName ->
    unless (any (\Argument {_name} -> _name == varName) args) $
      tell [UnknownIdentifier (getVal varName) (getPos varName)]
  RTSpace -> pure ()
  RTRun modeName terms -> do
    maybe (pure ()) checkMode modeName
    mapM_ (checkRuleTerm args) terms

checkMode :: MState m => Posed Text -> m ()
checkMode modeName = do
  mode <- use (definitions . modes . at (getVal modeName))
  case mode of
    Just _ -> pure ()
    Nothing -> tell [UndefinedMode (getVal modeName) (getPos modeName)]

procSort :: DefSort -> m ()
procSort = undefined

lens modeName = definitions . inModes . at (getVal modeName) . anon (mempty <$ modeName) null . h
  where
    h g pa = sequencePosed $ g <$> pa

procRuleDef :: forall m. MState m => Posed ModeName -> DefRule -> m ()
procRuleDef modeName DefRule {_match, _rule} = do
  i <- use $ lens modeName . rules . to M.keysSet . to S.lookupMax . to (fmap succ) . non 0
  lens modeName . rules %= M.insert i _rule
  tokAlts <- pmExpToTokenAlts i _match
  tokens <>= tokAlts

pmExpToTokenAlts :: forall m. MState m => Id -> PatMatchExp -> m [Token]
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
      (Maybe (Posed VarName), [BlackWhiteSetList Char]) ->
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
           in case getVal <$> curVar of
                Nothing -> f str'
                Just varName -> M.insert varName curStr (f str')

toSet :: BlackWhiteSet c -> S.Set c
toSet (WhiteSet s) = s
toSet (BlackSet _) = error "Black set" -- TODO: fix it

-- | Make a 'BlackWhiteSetList's equivalent (as a disjunction) to 'SortExp'
expToBWSetListAlts :: forall m. MState m => SortExp -> m [BlackWhiteSetList Char]
expToBWSetListAlts = \case
  SEString str -> pure [Tok.fromConcrete (T.unpack $ getVal str)]
  SESpace -> pure [[Tok.singleton ' ']]
  SESort sortName -> do
    mz <- use $ definitions . sorts . at (getVal sortName)
    case mz of
      Nothing -> do
        tell [UndefinedSort (getVal sortName) (getPos sortName)]
        pure []
      Just toks -> pure $ getVal toks
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
  forall m a.
  (MState m, HasName a (Posed Text)) =>
  Lens' Definitions (Map Text a) ->
  DefType ->
  a ->
  m ()
addDef lens deft val = do
  mval' <- use $ definitions . lens . at (val ^. name . to getVal)
  case mval' of
    Nothing -> definitions . lens %= M.insert (val ^. name . to getVal) val
    Just val' ->
      tell [MultipleDecl deft (getVal $ val ^. name) (getPos $ val ^. name) (getPos $ val' ^. name)]

instance HasDefinitions State Definitions where
  {-# INLINE definitions #-}
  definitions f_a1ma8U (State x1_a1ma8V x3_a1ma8X) =
    fmap
      (`State` x3_a1ma8X)
      (f_a1ma8U x1_a1ma8V)

class HasTokens s a | s -> a where
  tokens :: Lens' s a

instance HasTokens State [Token] where
  {-# INLINE tokens #-}
  tokens f_a1ma96 (State x1_a1ma97 x3_a1ma99) =
    fmap
      (State x1_a1ma97)
      (f_a1ma96 x3_a1ma99)
