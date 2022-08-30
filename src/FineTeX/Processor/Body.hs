module FineTeX.Processor.Body where

import Control.Monad (join, (>=>))
import Control.Monad.Extra (concatMapM)
import Control.Monad.RWS (MonadReader, MonadState (get, put), MonadWriter (tell), asks)
import Data.Generics.Product ()
import Data.List.Extra (intercalate, repeatedly)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils (Posed, getVal)
import FineTeX.Processor.Syntax
import FineTeX.Processor.Tokenizer (TokenizeError (NoWayTokenize), tokenize)
import FineTeX.Utils (localState, spanMaybe)
import Optics (At (..), Ixed (..), non, preview, to, (%), (^.), (^?))
import Prelude hiding (Word)

type MonadM m =
  ( MonadReader Definitions m,
    MonadState ModeName m,
    MonadWriter [TokenizeError Id Char] m
  )

-- | Document after substitution environments' and prefs' definitions
data WordDocElement word
  = -- | ~ 'GDocParagraph'
    WDocParagraph [word]
  | -- | First block contains @\@Begin@ option, last --- @\@End@
    WDocEnvironment ModeName [word] (EnvBody (WordDocElement word)) [word]
  | -- | First block contains @\@Pref@ option, last --- @\@Suff@ and
    -- (for every pref item, excluding last) @\@Sep@
    WDocPrefItem [word] [WordDocElement word] [word]
  | -- | ~'GDocEmptyLine'
    WDocEmptyLine
  deriving (Eq, Show)

-- | 'Word' without 'WWord' & 'WMode' constructors
data Word'
  = -- | String, that will be printed in output as it is, without any processing
    WString' (Posed Text)
  | -- | Space will be printed as space or eol
    WSpace'
  | -- | Group of words. Printer will try not to split it between lines
    WGroup' [Word']
  deriving (Eq, Show)

-- | Process body in two stages:
--
-- 1. todo
-- 2. todo
processBody :: MonadM m => [DocElement] -> m [WordDocElement Word']
processBody = bodyToWords >=> wordsToWords'

bodyToWords :: MonadM m => [DocElement] -> m [WordDocElement Word]
bodyToWords = (localState . mapM elToWord) . groupPrefs

wordsToWords' :: MonadM m => [WordDocElement Word] -> m [WordDocElement Word']
wordsToWords' = mapM elReplaceTokens

data GroupDocElement
  = -- | Paragraph elements, not split on lines (eols are replaced by WordSpace)
    GDocParagraph [ParEl]
  | -- | Environment
    GDocEnvironment (Posed Text) [ArgVal] (EnvBody GroupDocElement)
  | -- | Group of prefs
    GDocPrefGroup (Posed Text) [([ArgVal], [GroupDocElement])]
  | -- | Empty line
    GDocEmptyLine
  deriving (Eq, Show)

-- | Group prefixes and unline paragraphs.
groupPrefs :: [DocElement] -> [GroupDocElement]
groupPrefs = repeatedly h . removeComments
  where
    removeComments = filter $ \case
      DocCommentLine _ -> False
      _ -> True

    h [] = undefined -- todo: replace 'repeatedly` ~~> repeatedlyNE
    h (el : els) = case el of
      DocParLine _ ->
        let (lns, els'') = spanMaybe h (el : els)
            h (DocParLine ln') = Just ln'
            h _ = Nothing
         in (GDocParagraph $ intercalate [ParText [ParSpace]] lns, els'')
      DocEnvironment name argvs els' ->
        (GDocEnvironment name argvs (mapEnvBody groupPrefs els'), els)
      DocPref name _ _ ->
        let (prefs, els'') = spanMaybe (h name) (el : els)
            h name (DocPref name' argvs els')
              | name == name' = Just (argvs, groupPrefs els')
              | otherwise = Nothing
            h _ _ = Nothing
         in (GDocPrefGroup name prefs, els'')
      DocEmptyLine ->
        (GDocEmptyLine, els)
      DocCommentLine _ ->
        error "Comment found, removeComments should be called before this function"

elToWord :: MonadM m => GroupDocElement -> m (WordDocElement Word)
elToWord = \case
  GDocParagraph parels -> WDocParagraph <$> concatMapM parElToWords parels
  GDocEnvironment name argvs body -> do
    defs <- curModeDefs
    let env = defs ^. #envs % at (getVal name) % non (error "unexpected env")
        beg' = substArgs' (env ^. #args) argvs (env ^. #begin)
        end' = substArgs' (env ^. #args) argvs (env ^. #end)
    curMode <- get
    let mode = fromMaybe curMode $ env ^? #inner % #_NoVerb % #innerModeName % to getVal
    body' <- localState $ do
      put mode
      mapMEnvBody (mapM elToWord) body
    return $ WDocEnvironment mode beg' body' end'
  GDocPrefGroup name items -> do
    defs <- curModeDefs
    let pref = defs ^. #prefs % at (getVal name) % non (error "unexpected pref")
    let mode = pref ^. #innerModeName % to getVal
    let mkWords (isLast, (argvs, els)) = do
          let beg' = substArgs' (pref ^. #args) argvs (pref ^. #pref)
              end' = substArgs' (pref ^. #args) argvs (pref ^. #suf <> if isLast then [] else pref ^. #sep)
          els' <- mapM elToWord els
          return $ WDocPrefItem beg' els' end'
    items' <- localState $ do
      put mode
      mapM mkWords (zipWith (\i el -> (i == length items, el)) [1 ..] items)
    return $
      WDocEnvironment
        mode
        (pref ^. #begin % to (substArgs' [] []))
        (NoVerbBody items')
        (pref ^. #end % to (substArgs' [] []))
  GDocEmptyLine -> return WDocEmptyLine

parElToWords :: MonadM m => ParEl -> m [Word]
parElToWords = \case
  ParText ws -> pure $
    flip map ws $ \case
      ParWord s -> WWord s
      ParSpace -> WSpace
  ParInline name parEls -> do
    defs <- curModeDefs
    let inl = defs ^. #inlines % at name % non (error "unexpected inline")
    let mode = inl ^. #innerModeName % to getVal
    parEls' <- localState $ do
      put mode
      mapM parElToWords parEls
    return
      [ WMode mode $
          inl ^. #begin % to (substArgs' [] [])
            <> [WGroup $ join parEls']
            <> inl ^. #end % to (substArgs' [] [])
      ]

-- | Term of the right part of rules, sections of prefs and envs
data RuleTerm'
  = -- | String constant
    RTString' (Posed Text)
  | -- | Space symbol
    RTSpace'
  | -- | Rule terms need to be processed in particular mode
    -- (if Nothing, current mode is used)
    RTRun' (Maybe (Posed ModeName)) [RuleTerm']
  deriving (Eq, Show)

substArgs' :: [Argument] -> [ArgVal] -> [RuleTerm] -> [Word]
substArgs' args argvs = map ruleTerm'ToWord . substArgs args argvs

substArgs :: [Argument] -> [ArgVal] -> [RuleTerm] -> [RuleTerm']
substArgs args argvs = map repl
  where
    replMap = M.fromList $ zip ((\Argument {name} -> getVal name) <$> args) argvs
    repl :: RuleTerm -> RuleTerm'
    repl = \case
      RTString s -> RTString' s
      RTVar varName -> case M.lookup (getVal varName) replMap of
        Nothing -> error $ "Unexpected varriable " <> show varName -- TODO: fix it
        Just (AVString s) -> RTString' s
        Just (AVSort _) -> error "Sort arguments not realized yet" -- TODO: fix it
      RTSpace -> RTSpace'
      RTRun mode rts -> RTRun' mode (repl <$> rts)

ruleTerm'ToWord :: RuleTerm' -> Word
ruleTerm'ToWord = \case
  RTString' s -> WString s
  RTSpace' -> WSpace
  RTRun' Nothing rts -> WGroup (h <$> rts)
  RTRun' (Just mode) rts -> WMode (getVal mode) (h <$> rts)
  where
    h = \case
      RTString' s -> WWord s
      RTSpace' -> WSpace
      RTRun' _ _ -> error "Unexpected run" -- TODO: fix it

curModeDefs :: MonadM m => m InModeDefs
curModeDefs = do
  modeName <- get
  defs <- asks $ preview $ #inModes % ix modeName % to getVal
  return $ fromMaybe (error $ "Undefined mode" <> show modeName) defs

elReplaceTokens :: MonadM m => WordDocElement Word -> m (WordDocElement Word')
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

replaceTokens :: forall m. MonadM m => Word -> m [Word']
replaceTokens = \case
  WString s -> pure [WString' s]
  WWord w -> do
    defs <- curModeDefs
    let rls = defs ^. #rules
    ws' <- case tokenize (defs ^. #tokMap) (T.unpack $ getVal w) of
      Left NoWayTokenize {} ->
        pure [WString w]
      Left es ->
        tell [es] >> pure []
      Right replMaps ->
        pure $ concatMap (\(i, m) -> concatMap (repl m) (rls M.! i)) replMaps
    join <$> mapM replaceTokens ws'
  WSpace -> pure [WSpace']
  WMode mode ws -> localState $ do
    put mode
    join <$> mapM replaceTokens ws
  WGroup ws -> do
    ws' <- join <$> mapM replaceTokens ws
    return [WGroup' ws']
  where
    repl :: M.Map VarName String -> RuleTerm -> [Word]
    repl m = \case
      RTString ps -> [WString ps]
      RTVar pv -> [WString (T.pack . (m M.!) <$> pv)]
      RTSpace -> [WSpace]
      RTRun _ _ -> error "Run non realized"

subst :: M.Map VarName String -> RuleTerm -> Word
subst substMap = \case
  RTString s -> WString s
  RTVar var -> WString (T.pack . (substMap M.!) <$> var)
  RTSpace -> WSpace
  RTRun _ _ -> error "Not realized" -- TODO: realize it
