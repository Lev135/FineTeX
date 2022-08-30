{-# LANGUAGE PartialTypeSignatures #-}

module FineTeX.FineTeX
  ( -- * Type aliases for documents and definitions
    Document,
    Definitions,
    InitDocument,
    WordDocument,
    Word'Document,

    -- * Parsing
    ParseSourceError (..),
    ParseT,
    runParseT,
    parseDocSource,
    parseDefSource,

    -- * Functions for debugging. Processing document in 3 stages
    parseDocSourceInit,
    initDocToWordDoc,
    wordDocToWord'Doc,

    -- * Pretty-printing
    PrintOpts (..),
    PrefTabMode (..),
    makeStream,

    -- * Rendering
    renderText,
    renderIO,
  )
where

import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), liftEither, runExceptT)
import Control.Monad.RWS (MonadWriter (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalState, evalStateT, runState)
import Control.Monad.Writer (WriterT (runWriterT), execWriterT, runWriter)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.Parser.Body (defaultState, pBody)
import FineTeX.Parser.Definitions (pDefBlock, pImportFilenames)
import FineTeX.Parser.Syntax (DocElement, Word)
import FineTeX.Parser.Utils (Posed (..))
import FineTeX.Printer (Ann (..), PrefTabMode (..), PrintOpts (..), prettyDoc)
import FineTeX.Processor.Body (Word', WordDocElement, bodyToWords, processBody, wordsToWords')
import FineTeX.Processor.Definitions (ProcessDefsError, initState, processDefs)
import qualified FineTeX.Processor.Definitions as ProcDefs
import FineTeX.Processor.Syntax (Definitions, Id)
import FineTeX.Processor.Tokenizer (TokenizeError)
import FineTeX.Utils (Pos, PrettyErr (..), Sources, prettyPos, renderErrors, withError)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as PR
import System.IO (Handle)
import Text.Megaparsec (MonadParsec (eof, getParserState), ParseErrorBundle, State, errorBundlePretty, parse, runParser', runParserT')
import Prelude hiding (Word)

-- | Alias for processed body of FineTeX document
type Document = [WordDocElement Word']

-- Type aliases for debug processing

-- | Alias for init document (DEBUG)
type InitDocument = [DocElement]

-- | Alias for word document (DEBUG)
type WordDocument = [WordDocElement Word]

-- | Alias for word' document (DEBUG)
type Word'Document = Document

-- Reading

data ParseSourceError
  = OpenFileError FilePath String
  | ParseError (ParseErrorBundle Text Void)
  | ImportError FilePath Pos ParseSourceError
  | ProcessError [ProcessDefsError]
  | ProcessBodyError [TokenizeError Id Char]

instance PrettyErr ParseSourceError where
  prettyErr src e = case e of
    OpenFileError file e ->
      "Unable to open file '" <> P.pretty file <> "': " <> P.pretty (show e)
    ParseError peb -> "Parse error: " <> P.pretty (errorBundlePretty peb)
    ImportError file p rde ->
      P.vsep
        [ prettyPos src ("In '" <> T.pack file <> "' imported from here") p,
          prettyErr src rde
        ]
    ProcessError procErrs ->
      P.vsep (prettyErr src <$> procErrs)
    ProcessBodyError errs ->
      P.vsep . map (P.pretty . show) $ errs

-- | Transformer used by 'parseDocSource' & 'parseDefSource'
type ParseT m = ExceptT ParseSourceError (WriterT Sources m)

-- | Run monad transformer from 'parseDocSource' and 'parseDefSource' functions
runParseT ::
  Monad m =>
  ParseT m a ->
  m (Either Text a)
runParseT ma = do
  (res, srcs) <- runWriterT . runExceptT $ ma
  return $ first (renderErrors srcs . (: [])) res

parseDocSourceInit ::
  (Monad m) =>
  -- | Function for reading files.
  -- Arguments: base (can be used if path is relative) and path to the file
  -- return either error message (string) or contents of file and base for it
  -- for processing it's imports
  (base -> FilePath -> m (Either String (Text, base))) ->
  -- | Base. Used for calling 'readFile' function. See comments above
  base ->
  -- | Source file path
  FilePath ->
  ParseT m (Definitions, InitDocument)
parseDocSourceInit readFile base fileName = do
  (defs, state) <- parseDefsImpl readFile base fileName
  doc <-
    liftEither $
      first ParseError $
        snd . flip evalState defaultState . flip runReaderT defs . flip runParserT' state $
          (pBody <* eof)
  return (defs, doc)

initDocToWordDoc ::
  (Monad m) =>
  -- | Definitions, returned by 'parseDocSourceInit' function
  Definitions ->
  -- | InitDocument, returned by 'parseDocSourceInit' function
  InitDocument ->
  ParseT m WordDocument
initDocToWordDoc defs doc = do
  let (doc', errs) =
        runWriter . flip evalStateT (defaultState ^. #curModeName) . flip runReaderT defs $
          bodyToWords doc
  case errs of
    [] -> return doc'
    _ -> throwError $ ProcessBodyError errs

wordDocToWord'Doc ::
  (Monad m) =>
  -- | Definitions, returned by 'parseDocSourceInit' function
  Definitions ->
  -- | WordDocument, returned by 'initDocToWordDoc' function
  WordDocument ->
  ParseT m Word'Document
wordDocToWord'Doc defs doc = do
  let (doc', errs) =
        runWriter . flip evalStateT (defaultState ^. #curModeName) . flip runReaderT defs $
          wordsToWords' doc
  case errs of
    [] -> return doc'
    _ -> throwError $ ProcessBodyError errs

parseDocSource ::
  (Monad m) =>
  -- | Function for reading files.
  -- Arguments: base (can be used if path is relative) and path to the file
  -- return either error message (string) or contents of file and base for it
  -- for processing it's imports
  (base -> FilePath -> m (Either String (Text, base))) ->
  -- | Base. Used for calling 'readFile' function. See comments above
  base ->
  -- | Source file path
  FilePath ->
  ParseT m Document
parseDocSource readFile base fileName = do
  (defs, state) <- parseDefsImpl readFile base fileName
  doc <-
    liftEither $
      first ParseError $
        snd . flip evalState defaultState . flip runReaderT defs . flip runParserT' state $
          (pBody <* eof)
  let (doc', errs) =
        runWriter . flip evalStateT (defaultState ^. #curModeName) . flip runReaderT defs $
          processBody doc
  case errs of
    [] -> return doc'
    _ -> throwError $ ProcessBodyError errs

parseDefSource ::
  (Monad m) =>
  -- | Function for reading files.
  -- Arguments: base (can be used if path is relative) and path to the file
  -- return either error message (string) or contents of file and base for it
  -- for processing it's imports
  (base -> FilePath -> m (Either String (Text, base))) ->
  -- | Base. Used for calling 'readFile' function. See comments above
  base ->
  -- | Source file path
  FilePath ->
  ParseT m Definitions
parseDefSource readFile base fileName = do
  (defs, state) <- parseDefsImpl readFile base fileName
  liftEither $ first ParseError $ runParserT' eof state
  return defs

-- | Auxillary function for parsing FineTeX source files
parseDefsImpl ::
  (Monad m) =>
  -- | Function for reading files.
  -- Arguments: base (can be used if path is relative) and path to the file
  -- return either error message (string) or contents of file and base for it
  -- for processing it's imports
  (base -> FilePath -> m (Either String (Text, base))) ->
  -- | Base. Used for calling 'readFile' function. See comments above
  base ->
  -- | Source file path
  FilePath ->
  ParseT m (Definitions, State Text Void)
parseDefsImpl readFile base fileName = do
  mfile <- lift . lift $ readFile base fileName
  (file, base') <- liftEither $ first (OpenFileError fileName) mfile
  tell $ M.singleton fileName (T.lines file)
  (impFNames, state') <-
    liftEither $
      first ParseError $
        parse ((,) <$> pImportFilenames <*> getParserState) fileName file
  defs <- do
    let h pf =
          withError (ImportError (getVal pf) (getPos pf)) $
            parseDefSource readFile base' $ getVal pf
    mconcat <$> mapM h impFNames
  (defs', state'') <- do
    let (state'', mdefs') = runParser' pDefBlock state'
    defs' <- liftEither $ first ParseError mdefs'
    return (defs', state'')
  defs'' <- do
    let (errs, ProcDefs.State {definitions = defs''}) = flip runState (initState defs) . execWriterT $ processDefs defs'
    case errs of
      [] -> return defs''
      _ -> throwError $ ProcessError errs
  return (defs'', state'')

-- Printing

-- | Alias for PrettyPrinter stream used by FineTeX
type FineTeXStream = P.SimpleDocStream Void

makeStream :: PrintOpts -> Int -> Document -> FineTeXStream
makeStream printOpts pageWidth doc =
  h $ P.layoutSmart renderOpts $ prettyDoc printOpts doc
  where
    renderOpts = P.defaultLayoutOptions {P.layoutPageWidth = P.AvailablePerLine pageWidth 1.0}
    h :: P.SimpleDocStream Ann -> P.SimpleDocStream Void
    h sds = case sds of
      P.SFail -> P.SFail
      P.SEmpty -> P.SEmpty
      P.SChar c rest -> P.SChar c $ h rest
      P.SText l t rest ->
        if noIndNext sds
          then h rest
          else P.SText l t $ h rest
      P.SLine i rest ->
        if noIndNext sds
          then P.SLine 0 $ h rest
          else P.SLine i $ h rest
      P.SAnnPush NoIndent rest -> remInd rest
      P.SAnnPop rest -> h rest
    remInd :: P.SimpleDocStream Ann -> P.SimpleDocStream Void
    remInd = \case
      P.SFail -> P.SFail
      P.SEmpty -> P.SEmpty
      P.SChar c rest -> P.SChar c $ remInd rest
      P.SText l t rest -> P.SText l t $ remInd rest
      P.SLine _ rest -> P.SLine 0 $ remInd rest
      P.SAnnPush _ rest -> remInd rest
      P.SAnnPop rest -> h rest
    noIndNext :: P.SimpleDocStream Ann -> Bool
    noIndNext = \case
      P.SAnnPush NoIndent _ -> True
      P.SLine _ rest -> noIndNext rest
      P.SText _ t rest -> T.all isSpace t && noIndNext rest
      _ -> False

renderText :: FineTeXStream -> Text
renderText = PR.renderStrict

renderIO :: Handle -> FineTeXStream -> IO ()
renderIO = PR.renderIO
