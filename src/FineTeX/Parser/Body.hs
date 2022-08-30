{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Parser.Body where

import Control.Lens (Ixed (ix), makeLenses, to, use, view, (.=), (^.))
import Control.Monad (void)
import Control.Monad.RWS (MonadReader, MonadState)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.Dbg.MonadParsecDbg (MonadParsecDbg (..))
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils hiding (ParserM)
import FineTeX.Processor.Syntax
import FineTeX.Utils (localState)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

data MState = MState
  { _curModeName :: Text,
    _closeInline :: Maybe Text,
    _prefIndent :: Maybe Pos
  }
  deriving (Show)

makeLenses ''MState

type ParserM m =
  ( MonadFail m,
    MonadParsec Void Text m,
    MonadParsecDbg m,
    MonadReader Definitions m,
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
pBody :: ParserM m => m [DocElement]
pBody = L.nonIndented scn (pElements False pos1)

-- | Parse many (zero or more) elements until the indentation
--   will be less then @ind@
--
-- There are two modes of parsing elements:
--   - normal (@noPref@ is false).
--     All paragraphs, environments and comments must have indentation @ind@.
--     Prefs must have greater indentation
--   - no-prefix mode (@noPref@ is true).
--     Paragraphs, environments and comments can have arbitrary
--     indentation, grater or equal to ind.
--     Nothing is considered to be a prefix.
pElements :: ParserM m => Bool -> Pos -> m [DocElement]
pElements noPref ind = manyTill pel pend
  where
    pel
      | noPref = pEmptyLine <|> pEnvironment <|> pParLine <|> pCommentLine
      | otherwise =
        pEmptyLine <|> do
          ind' <- L.indentLevel
          if ind == ind'
            then pCommentLine <|> pEnvironment <|> pParLine
            else pPref
    pEmptyLine = DocEmptyLine <$ eol
    pend = try . lookAhead $ spn *> (eof <|> void (L.indentGuard sp LT ind))

-- | Parse environment body in verb mode until the indentation
--   become less than ind
--
-- Empty lines at the end of environment will not be consumed
pVerb :: ParserM m => Pos -> m [Posed Text]
pVerb ind = manyTill (pEmptyLine <|> pNonEmptyLine) pend
  where
    pEmptyLine = withPos ("" <$ eol) <* sp
    pNonEmptyLine = do
      ind' <- L.indentLevel
      s <- withPos $ T.pack <$> manyTill anySingle eolf
      return $ (T.replicate (unPos ind' - unPos ind) " " <>) <$> s
    pend = try . lookAhead $ spn *> (eof <|> void (L.indentGuard sp LT ind))

pCommentLine :: ParserM m => m DocElement
pCommentLine = DocCommentLine <$> withPos lineComment <* eol

-- | Parse a line of paragraph. The first symbol can not be '@'.
-- Empty lines are not allowed
pParLine :: ParserM m => m DocElement
pParLine = do
  notFollowedBy $ char '@'
  DocParLine <$> some pParEl <* sc <* eolf <* sp

pEnvironment :: ParserM m => m DocElement
pEnvironment = do
  ind <- L.indentLevel
  defs <- curModeDefs
  (name, env) <-
    parseMapEl (defs ^. envs) "environment" (char '@' *> pIdentifierL)
  argvs <- mapM pArgV $ env ^. args
  eolf
  ind' <- getBlockInnerIndent ind
  body <- case env ^. inner of
    Verb verbInd -> do
      case ind' of
        Inf -> pure $ VerbBody verbInd []
        Pos ind' -> VerbBody verbInd <$> pVerb ind'
    NoVerb EnvNoVerb {_innerModeName, _noPrefInside} ->
      case ind' of
        Inf -> pure $ NoVerbBody []
        Pos ind' -> localState $ do
          curModeName .= getVal _innerModeName
          NoVerbBody <$> pElements _noPrefInside ind'
  return $ DocEnvironment name argvs body

pPref :: ParserM m => m DocElement
pPref = do
  ind <- L.indentLevel
  defs <- curModeDefs
  (name, pref) <-
    parseMapEl (defs ^. prefs) "prefix" pPrefixL
  argvs <- mapM pArgV $ pref ^. args
  ind' <- getBlockInnerIndent ind
  case ind' of
    Inf -> DocPref name argvs [] <$ eol
    (Pos ind') -> DocPref name argvs <$> pElements (pref ^. noPrefInside) ind'

-- | Parse non-empty block of text or inline
pParEl :: ParserM m => m ParEl
pParEl = pParText <|> pInline

-- | Parse non-empty sequence of words and spaces
pParText :: ParserM m => m ParEl
pParText = ParText <$> some (ParWord <$> pParWord <|> ParSpace <$ sp1)

-- | Parse non-empty sequence of symbols without spaces
-- and inline beginnings/endings in it
pParWord :: ParserM m => m (Posed Text)
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
  -- 'notFollowedBy' is necessary, because 'someTill'
  -- does not fail if endP consumes the first character
  notFollowedBy (lookAhead endP)
  T.pack <$> someTill anySingle (lookAhead endP)

-- | Parse one of available open inline sequences of characters,
-- than paragraph elements in inner mode and corresponding close inline sequence
pInline :: forall m. ParserM m => m ParEl
pInline = do
  defs <- curModeDefs
  let openInls = defs ^. inlines . to (map snd . M.toList)
  choice $ map mkInlP openInls
  where
    mkInlP :: DefInline -> m ParEl
    mkInlP DefInline {_innerModeName, _borders = (b, e)} = do
      string (getVal b)
      els <- localState $ do
        curModeName .= getVal _innerModeName
        closeInline .= Just (getVal e)
        many pParEl
      string (getVal e)
      return $ ParInline (getVal b) els

-- | Parse argument value: this will be changed soon
pArgV :: ParserM m => Argument -> m ArgVal
pArgV arg = label (prettyArg arg) $ case arg ^. kind of
  AKString -> AVString <$> pStringLiteralL
  AKSort _ -> AVSort <$> pWordL

prettyArg :: Argument -> String
prettyArg arg = "(" <> nameS <> " : " <> typeS <> ")"
  where
    nameS = arg ^. name . to (T.unpack . getVal)
    typeS = case arg ^. kind of
      AKString -> "String"
      AKSort _ -> "Word"

-- | Get 'InModeDefs' block appropriate to current mode
curModeDefs :: ParserM m => m InModeDefs
curModeDefs = do
  modeName <- use curModeName
  view $ inModes . ix modeName . to getVal

data PosInf = Inf | Pos Pos
  deriving (Eq, Show)

instance Ord PosInf where
  _ <= Inf = True
  Inf <= (Pos _) = False
  (Pos a) <= (Pos b) = a <= b

-- | Get maximal indentation of all lines in block starting from current
-- position and ending on a non-empty string with indentation less then 'ind0'
-- or at the end of file.
--
-- If block is empty @PosInf@ is returned
--
-- _NB. Trailing empty lines are not excluded, but have no influence on result,
-- since @PosInf@ is grater than any significant indentation_
getBlockInnerIndent :: ParserM m => Pos -> m PosInf
getBlockInnerIndent ind0 =
  lookAhead . fmap (foldr min Inf) . many $
    emptyLine <|> nonEmptyLine
  where
    emptyLine = Inf <$ eol <* sp
    nonEmptyLine =
      Pos <$> L.indentGuard sc GT ind0
        <* someTill anySingle eolf
        <* sp
