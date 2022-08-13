{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module FineTeX.Processor.Body where

import Control.Lens (At (..), Ixed (..), non, to, view, (^.), (^?))
import Control.Monad (join)
import Control.Monad.Extra (concatMapM)
import Control.Monad.RWS (MonadReader, MonadState (get, put), MonadWriter)
import Data.List.Extra (intercalate, repeatedly)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax
import FineTeX.Processor.Syntax
import FineTeX.Processor.Tokenizer (tokenize)
import FineTeX.Utils (Box (unBox), localState, spanMaybe)
import Prelude hiding (Word)

type MonadM m p =
  ( Box p,
    MonadReader (Definitions p) m,
    MonadState ModeName m,
    MonadWriter [[[Text]]] m
  )

-- | Document after substitution environments' and prefs' definitions
data WordDocElement word p
  = -- | ~ 'GDocParagraph'
    WDocParagraph [word p]
  | -- | First block contains @\@Begin@ option, last --- @\@End@
    WDocEnvironment ModeName [word p] (EnvBody (WordDocElement word) p) [word p]
  | -- | First block contains @\@Pref@ option, last --- @\@Suff@ and
    -- (for every pref item, excluding last) @\@Sep@
    WDocPrefItem [word p] [WordDocElement word p] [word p]
  | -- | ~'GDocEmptyLine'
    WDocEmptyLine

-- | 'Word' without 'WWord' & 'WMode' constructors
data Word' p
  = -- | String, that will be printed in output as it is, without any processing
    WString' (p Text)
  | -- | Space will be printed as space or eol
    WSpace'
  | -- | Group of words. Printer will try not to split it between lines
    WGroup' [Word' p]

deriving instance Box p => Eq (Word' p)

deriving instance Box p => Show (Word' p)

deriving instance (Show (word p), Box p) => Show (WordDocElement word p)

processBody :: MonadM m p => [DocElement p] -> m [WordDocElement Word' p]
processBody els = do
  let gels = groupPrefs . takeOutEmptyLines $ els
  wels <- localState $ mapM elToWord gels
  mapM elReplaceTokens wels

bodyToWords :: MonadM m p => [DocElement p] -> m [WordDocElement Word p]
bodyToWords els = do
  let gels = groupPrefs . takeOutEmptyLines $ els
  localState $ mapM elToWord gels

wordsToWords' :: MonadM m p => [WordDocElement Word p] -> m [WordDocElement Word' p]
wordsToWords' = mapM elReplaceTokens

-- | Take trailing empty lines out of environments and prefs
-- should be called before grouping
takeOutEmptyLines :: forall p. [DocElement p] -> [DocElement p]
takeOutEmptyLines els = case takeOutELs els of
  (els', True) -> els' <> [DocEmptyLine]
  (els', False) -> els'
  where
    takeOutELs :: [DocElement p] -> ([DocElement p], Bool)
    takeOutELs = foldr hh ([], False)

    hh :: DocElement p -> ([DocElement p], Bool) -> ([DocElement p], Bool)
    hh el ([], eol) = let (el', eol') = takeOutEL el in (el', eol || eol')
    hh el (xs@(x : _), eol) =
      let (el', eol') = takeOutEL el
       in case (eol', x) of
            (_, DocEmptyLine) -> (el : xs, eol)
            (True, _) -> (el' <> (DocEmptyLine : xs), eol)
            (False, _) -> (el' <> xs, eol)

    takeOutEL :: DocElement p -> ([DocElement p], Bool)
    takeOutEL el = case el of
      DocParagraph _ -> ([el], False)
      DocEnvironment _ _ (VerbBody _ _) -> ([el], False)
      DocEnvironment name argvs (NoVerbBody els) ->
        let (els', eol') = takeOutELs els
         in ([DocEnvironment name argvs (NoVerbBody els')], eol')
      DocPref name argvs els ->
        let (els', eol') = takeOutELs els
         in ([DocPref name argvs els'], eol')
      DocEmptyLine -> ([], True)

data GroupDocElement p
  = -- | Paragraph elements, not split on lines (eols are replaced by WordSpace)
    GDocParagraph [ParEl p]
  | -- | Environment
    GDocEnvironment (p Text) [ArgVal p] (EnvBody GroupDocElement p)
  | -- | Group of prefs
    GDocPrefGroup (p Text) [([ArgVal p], [GroupDocElement p])]
  | -- | Empty line
    GDocEmptyLine

-- | Group prefixes and unline paragraphs.
groupPrefs :: Box p => [DocElement p] -> [GroupDocElement p]
-- List must be NonEmpty according to realization of `repeatedly`
-- see https://github.com/ndmitchell/extra/issues/95 for more information
groupPrefs = repeatedly $ \(el : els) -> case el of
  DocParagraph lines ->
    (GDocParagraph $ intercalate [ParText [ParSpace]] lines, els)
  DocEnvironment name argvs els' ->
    (GDocEnvironment name argvs (mapEnvBody groupPrefs els'), els)
  DocPref name _ _ ->
    let (prefs, els'') = spanMaybe (h name) (el : els)
     in (GDocPrefGroup name prefs, els'')
  DocEmptyLine ->
    (GDocEmptyLine, els)
  where
    h name (DocPref name' argvs els')
      | name == name' = Just (argvs, groupPrefs els')
      | otherwise = Nothing
    h _ _ = Nothing

elToWord :: MonadM m p => GroupDocElement p -> m (WordDocElement Word p)
elToWord = \case
  GDocParagraph parels -> WDocParagraph <$> concatMapM parElToWords parels
  GDocEnvironment name argvs body -> do
    defs <- curModeDefs
    let env = defs ^. envs . at (unBox name) . non (error "unexpected env")
        beg' = substArgs' (env ^. args) argvs (env ^. begin)
        end' = substArgs' (env ^. args) argvs (env ^. end)
    state <- get
    let mode = fromMaybe state $ env ^? inner . _NoVerb . innerModeName . to unBox
    body' <- localState $ do
      put mode
      mapMEnvBody (mapM elToWord) body
    return $ WDocEnvironment mode beg' body' end'
  GDocPrefGroup name items -> do
    defs <- curModeDefs
    let pref = defs ^. prefs . at (unBox name) . non (error "unexpected pref")
    let mode = pref ^. innerModeName . to unBox
    let mkWords (isLast, (argvs, els)) = do
          let beg' = substArgs' (pref ^. args) argvs (pref ^. FineTeX.Parser.Syntax.pref)
              end' = substArgs' (pref ^. args) argvs (pref ^. suf <> if isLast then [] else pref ^. sep)
          els' <- mapM elToWord els
          return $ WDocPrefItem beg' els' end'
    items' <- localState $ do
      put mode
      mapM mkWords (zipWith (\i el -> (i == length items, el)) [1 ..] items)
    return $
      WDocEnvironment
        mode
        (pref ^. begin . to (substArgs' [] []))
        (NoVerbBody items')
        (pref ^. end . to (substArgs' [] []))
  GDocEmptyLine -> return WDocEmptyLine

parElToWords :: MonadM m p => ParEl p -> m [Word p]
parElToWords = \case
  ParText ws -> pure $
    flip map ws $ \case
      ParWord s -> WWord s
      ParSpace -> WSpace
  ParInline name parEls -> do
    defs <- curModeDefs
    let inl = defs ^. inlines . at name . non (error "unexpected inline")
    let mode = inl ^. innerModeName . to unBox
    parEls' <- localState $ do
      put mode
      mapM parElToWords parEls
    return
      [ WMode mode $
          inl ^. begin . to (substArgs' [] [])
            <> [WGroup $ join parEls']
            <> inl ^. end . to (substArgs' [] [])
      ]

-- | Term of the right part of rules, sections of prefs and envs
data RuleTerm' p
  = -- | String constant
    RTString' (p Text)
  | -- | Space symbol
    RTSpace'
  | -- | Rule terms need to be processed in particular mode
    -- (if Nothing, current mode is used)
    RTRun' (Maybe (p ModeName)) [RuleTerm' p]

substArgs' :: Box p => [Argument p] -> [ArgVal p] -> [RuleTerm p] -> [Word p]
substArgs' args argvs = map ruleTerm'ToWord . substArgs args argvs

substArgs :: forall p. Box p => [Argument p] -> [ArgVal p] -> [RuleTerm p] -> [RuleTerm' p]
substArgs args argvs = map repl
  where
    replMap = M.fromList $ zip ((\Argument {_name} -> unBox _name) <$> args) argvs
    repl :: RuleTerm p -> RuleTerm' p
    repl = \case
      RTString s -> RTString' s
      RTVar varName -> case M.lookup (unBox varName) replMap of
        Nothing -> error $ "Unexpected varriable " <> show varName -- TODO: fix it
        Just (AVString s) -> RTString' s
        Just (AVSort _) -> error "Sort arguments not realized yet" -- TODO: fix it
      RTSpace -> RTSpace'
      RTRun mode rts -> RTRun' mode (repl <$> rts)

ruleTerm'ToWord :: Box p => RuleTerm' p -> Word p
ruleTerm'ToWord = \case
  RTString' s -> WString s
  RTSpace' -> WSpace
  RTRun' Nothing rts -> WGroup (h <$> rts)
  RTRun' (Just mode) rts -> WMode (unBox mode) (h <$> rts)
  where
    h = \case
      RTString' s -> WWord s
      RTSpace' -> WSpace
      RTRun' _ _ -> error "Unexpected run" -- TODO: fix it

curModeDefs :: MonadM m p => m (InModeDefs p)
curModeDefs = do
  modeName <- get
  view $ inModes . ix modeName . to unBox

elReplaceTokens :: MonadM m p => WordDocElement Word p -> m (WordDocElement Word' p)
elReplaceTokens = \case
  WDocParagraph ws ->
    WDocParagraph . join <$> mapM replaceTokens ws
  WDocEnvironment mode bws body ews -> do
    bws' <- join <$> mapM replaceTokens bws
    body' <- localState $ do
      put mode
      flip mapMEnvBody body $ mapM elReplaceTokens
    ews' <- join <$> mapM replaceTokens ews
    return $ WDocEnvironment mode bws' body' ews'
  WDocPrefItem bws els ews -> do
    bws' <- join <$> mapM replaceTokens bws
    els' <- mapM elReplaceTokens els
    ews' <- join <$> mapM replaceTokens ews
    return $ WDocPrefItem bws' els' ews'
  WDocEmptyLine -> return WDocEmptyLine

replaceTokens :: forall m p. MonadM m p => Word p -> m [Word' p]
replaceTokens = \case
  WString s -> pure [WString' s]
  WWord w -> do
    defs <- curModeDefs
    let rls = defs ^. rules
    ws' <- case tokenize (defs ^. tokMap) (T.unpack $ unBox w) of
      Left _ ->
        pure []
      --     pure [WString w] --tell [es] >> pure []
      Right replMaps ->
        pure $ concatMap (\(i, m) -> concatMap (repl m) (rls M.! i)) replMaps
    --     pure $ (\(i, replMap) -> map (subst replMap) (rls ^?! ix i)) =<< replMaps
    join <$> mapM replaceTokens ws'
  WSpace -> pure [WSpace']
  WMode mode ws -> localState $ do
    put mode
    join <$> mapM replaceTokens ws
  WGroup ws -> do
    ws' <- join <$> mapM replaceTokens ws
    return [WGroup' ws']
  where
    repl :: M.Map VarName String -> RuleTerm p -> [Word p]
    repl m = \case
      RTString ps -> [WString ps]
      RTVar pv -> [WString (T.pack . (m M.!) <$> pv)]
      RTSpace -> [WSpace]
      RTRun _ _ -> error "Run non realized"

subst :: Box p => M.Map VarName String -> RuleTerm p -> Word p
subst substMap = \case
  RTString s -> WString s
  RTVar var -> WString (T.pack . (substMap M.!) <$> var)
  RTSpace -> WSpace
  RTRun _ _ -> error "Not realized" -- TODO: realize it
