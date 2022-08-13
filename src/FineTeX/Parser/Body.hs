{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Parser.Body where

import Control.Lens (Ixed (ix), makeLenses, to, use, view, (.=), (^.))
import Control.Monad (guard, void)
import Control.Monad.RWS (MonadReader, MonadState)
import Data.List.Extra (nubSort)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.Dbg.MonadParsecDbg (MonadParsecDbg (..))
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils hiding (ParserM)
import FineTeX.Processor.Syntax
import FineTeX.Utils (Box (..), PosC, localState)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L

data MState = MState
  { _curModeName :: Text,
    _closeInline :: Maybe Text,
    _prefIndent :: Maybe Pos
  }
  deriving (Show)

makeLenses ''MState

type ParserM m p =
  ( PosC p,
    MonadFail m,
    MonadParsec Void Text m,
    MonadParsecDbg m,
    MonadReader (Definitions p) m,
    MonadState MState m
  )

defaultState :: MState
defaultState =
  MState
    { _curModeName = "Normal",
      _closeInline = Nothing,
      _prefIndent = Nothing
    }

-- | Parse body of FineTeX document
pBody :: ParserM m p => m [DocElement Posed]
pBody = L.nonIndented scn (pElements False pos1)

-- | Parse many (zero or more) elements until the indentation
--   will be less then 'ind0'
--
-- There are two modes of parsing elements:
--   - normal ('noPref' is false).
--     All paragraphs and environments must have
--     minimal indentation in the block
--     Prefs must have greater indentation (equal for all of them)
--   - no-prefix mode ('noPref' is true).
--     Paragraphs and environments can have arbitrary
--     indentation, grater than ind0.
--     However indentation of different lines in one paragraph must be the same.
--     Nothing is considered to be a prefix.
pElements :: ParserM m p => Bool -> Pos -> m [DocElement Posed]
pElements noPref ind0 = do
  inds <- if noPref then pure [pos1] else getBlockIndents ind0
  manyTill (pel inds) pend
  where
    pel [] = DocEmptyLine <$ scn1
    pel [_] = (DocEmptyLine <$ scn1) <|> pEnvironment <|> pParagraph
    pel (p : pref : _) =
      (DocEmptyLine <$ scn1) <|> do
        ind <- L.indentLevel
        if
            | ind == p -> pEnvironment <|> pParagraph
            | ind == pref -> pPref
            | otherwise ->
              fail . unwords $
                [ "Incorrect indentation " <> sh ind <> ".",
                  "It should be " <> sh p <> " (for paragraphs/environments) or ",
                  sh pref <> " (for prefs)."
                ]
    sh = show . unPos
    pend = eof <|> void (notFollowedBy eol *> L.indentGuard sc LT ind0)

-- | Parse some lines of paragraph.
-- Lines must have the same indentation and the first symbol of the paragraph can not be '@'.
-- Empty lines are not allowed
pParagraph :: ParserM m p => m (DocElement Posed)
pParagraph = do
  assert $ notFollowedBy sp1
  lvl <- L.indentLevel
  els <- some $ do
    L.indentGuard sc EQ lvl
    notFollowedBy $ char '@'
    some pParEl <* sc <* eolf <* sc
  return $ DocParagraph els

pEnvironment :: ParserM m p => m (DocElement Posed)
pEnvironment = do
  ind <- L.indentLevel
  defs <- curModeDefs
  (name, env) <-
    parseMapEl (defs ^. envs) "environment" (char '@' *> pIdentifierL)
  argvs <- mapM pArgV $ env ^. args
  eolf *> sc
  body <- case env ^. inner of
    Verb verbInd ->
      VerbBody verbInd <$> pVerb (ind <> pos1)
    NoVerb EnvNoVerb {_innerModeName, _noPrefInside} ->
      localState $ do
        curModeName .= unBox _innerModeName
        NoVerbBody <$> pElements _noPrefInside (ind <> pos1)
  return $ DocEnvironment name argvs body

pPref :: ParserM m p => m (DocElement Posed)
pPref = do
  ind <- L.indentLevel
  defs <- curModeDefs
  (name, pref) <-
    parseMapEl (defs ^. prefs) "prefix" pPrefixL
  argvs <- mapM pArgV $ pref ^. args
  sc
  DocPref name argvs <$> pElements (pref ^. noPrefInside) (ind <> pos1)

-- | Parse non-empty block of text or inline
pParEl :: ParserM m p => m (ParEl Posed)
pParEl = pParText <|> pInline

-- | Parse non-empty sequence of words and spaces
pParText :: ParserM m p => m (ParEl Posed)
pParText = ParText <$> some (ParWord <$> pParWord <|> ParSpace <$ sp1)

-- | Parse non-empty sequence of symbols without spaces
-- and inline beginnings/endings in it
pParWord :: ParserM m p => m (Posed Text)
pParWord = withPos $ do
  defs <- curModeDefs
  let openInls = defs ^. inlines . to M.keys
  closeInl <- use closeInline
  let endP =
        choice
          (void . string <$> (openInls <> maybeToList closeInl))
          <|> sp1
          <|> eolf
          <|> void (char '%')
  lookAhead $ notFollowedBy endP
  T.pack <$> manyTill anySingle (lookAhead endP)

-- | Parse one of available open inline sequences of characters,
-- than paragraph elements in inner mode and corresponding close inline sequence
pInline :: forall m p. ParserM m p => m (ParEl Posed)
pInline = do
  defs <- curModeDefs
  let openInls = defs ^. inlines . to (map snd . M.toList)
  choice $ map mkInlP openInls
  where
    mkInlP :: DefInline p -> m (ParEl Posed)
    mkInlP DefInline {_innerModeName, _borders = (b, e)} = do
      string (unBox b)
      els <- localState $ do
        curModeName .= unBox _innerModeName
        closeInline .= Just (unBox e)
        many pParEl
      string (unBox e)
      return $ ParInline (unBox b) els

-- | Parse argument value: this will be changed soon
pArgV :: ParserM m p => Argument p -> m (ArgVal Posed)
pArgV arg = label (prettyArg arg) $ case arg ^. kind of
  AKString -> AVString <$> pStringLiteralL
  AKSort _ -> AVSort <$> pWordL

prettyArg :: Box p => Argument p -> String
prettyArg arg = "(" <> nameS <> " : " <> typeS <> ")"
  where
    nameS = arg ^. name . to (T.unpack . unBox)
    typeS = case arg ^. kind of
      AKString -> "String"
      AKSort _ -> "Word"

-- | Parse environment body in verb mode,
-- i. e. zero or more lines with greater than ind0 indentation or empty lines.
-- Empty lines at the end of environment will be consumed
-- (and so will be printed in the same number).
pVerb :: ParserM m p => Pos -> m [Posed Text]
pVerb ind0 = do
  inds <- getBlockIndents ind0
  manyTill (pel inds) pend
  where
    pel [] = withPos ("" <$ sc <* eol <* sc)
    pel (p : _) =
      withPos ("" <$ sc <* eol <* sc) <|> do
        ind <- L.indentLevel
        s <- withPos $ T.pack <$> manyTill anySingle eolf
        sc
        return $ (T.replicate (unPos ind - unPos p) " " <>) <$> s
    pend = eof <|> void (notFollowedBy eol *> L.indentGuard sc LT ind0)

-- | Get 'InModeDefs' block appropriate to current mode
curModeDefs :: ParserM m p => m (InModeDefs p)
curModeDefs = do
  modeName <- use curModeName
  view $ inModes . ix modeName . to unBox

-- | Get list of indentations of all lines in block starting from current position
-- end ending on a non-empty string with indentation less then 'ind0' or 'eof'.
getBlockIndents :: ParserM m p => Pos -> m [Pos]
getBlockIndents ind0 = lookAhead . fmap nubSort . (scn *>) . many $ do
  ind <- L.indentLevel
  guard $ ind >= ind0
  notFollowedBy eof
  manyTill anySingle eolf
  scn
  return ind
