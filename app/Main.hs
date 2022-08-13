{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad (when)
import Control.Monad.Identity (Identity (..))
import Data.List.Extra (spanEnd)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FineTeX.FineTeX
  ( PrefTabMode (..),
    PrintOpts (..),
    initDocToWordDoc,
    makeStream,
    parseDocSourceInit,
    renderIO,
    runParseT,
    wordDocToWord'Doc,
  )
import qualified IOUtils
import Options.Applicative (Alternative ((<|>)), readerError)
import qualified Options.Applicative as Opt
import System.Directory.Extra (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (IOMode (AppendMode, WriteMode), hClose, openFile)
import System.IO.Error (catchIOError)
import Text.Pretty.Simple (pHPrint, pPrint)
import Prelude hiding (writeFile)

data Stage = Stage
  { short :: Char,
    long :: String,
    description :: String,
    filename :: String
  }

stageInit :: Stage
stageInit =
  Stage
    { short = 'x',
      long = "doc",
      description = "initial document structure after parsing before processing",
      filename = "doc.dbg"
    }

stageWord :: Stage
stageWord =
  Stage
    { short = 'y',
      long = "word",
      description = "document structure after prefs and envs substritution",
      filename = "word.dbg"
    }

stageWord' :: Stage
stageWord' =
  Stage
    { short = 'z',
      long = "word'",
      description = "document structure after command substitution",
      filename = "word'.dbg"
    }

data DbgOptions = DbgOptions
  { dbgInit, dbgWord, dbgWord' :: Bool,
    dbgMode :: DbgMode
  }
  deriving (Show)

data DbgMode = NoDbg | DbgStd | DbgFile FilePath | DbgDir FilePath
  deriving (Show)

processFile :: Options -> IO ()
processFile Options {inpFile, outpFile, pageWidth, printOpts, dbgOpts} = do
  let DbgOptions {dbgInit, dbgWord, dbgWord', dbgMode} = dbgOpts
  print dbgOpts
  dir <- getCurrentDirectory
  mdoc <- runParseT $ parseDocSourceInit rfile dir inpFile
  case mdoc of
    Left e -> IOUtils.putStrLn $ T.unpack e
    Right (defs, doc) -> do
      when dbgInit $
        dbg dbgMode stageInit doc
      print "*"
      case runIdentity $ runParseT $ initDocToWordDoc defs doc of
        Left e -> IOUtils.putStrLn $ T.unpack e
        Right wdoc -> do
          when dbgWord $
            dbg dbgMode stageWord wdoc
          print "*"
          case runIdentity $ runParseT $ wordDocToWord'Doc defs wdoc of
            Left e -> IOUtils.putStrLn $ T.unpack e
            Right w'doc -> do
              print "?"
              when dbgWord' $
                dbg dbgMode stageWord' w'doc
              print "*"
              outp <- openFile outpFile WriteMode
              renderIO outp . makeStream printOpts pageWidth $ w'doc
              hClose outp
  where
    dbg dbgMode stage a = case dbgMode of
      NoDbg -> return ()
      DbgStd -> pPrint a
      DbgFile s -> do
        createDirectoryIfMissing True (takeDirectory s)
        h <- openFile s AppendMode
        pHPrint h a
        hClose h
      DbgDir s -> do
        createDirectoryIfMissing True s
        h <- openFile (s </> filename stage) WriteMode
        pHPrint h a
        hClose h
    rfile base path =
      ( do
          let path' = if isAbsolute path then path else base </> path
          res <- TIO.readFile path'
          return $ Right (res, takeDirectory path')
      )
        `catchIOError` (pure . Left . show)

data Options = Options
  { inpFile :: FilePath,
    outpFile :: FilePath,
    pageWidth :: Int,
    printOpts :: PrintOpts,
    dbgOpts :: DbgOptions
  }

dbgOptions :: Opt.Parser DbgOptions
dbgOptions = do
  dbgMode <-
    dbgStd <|> DbgFile <$> dbgFile <|> DbgDir <$> dbgDir <|> pure NoDbg
  dbgInit <- mkOpt stageInit
  dbgWord <- mkOpt stageWord
  dbgWord' <- mkOpt stageWord'
  pure $ DbgOptions {..}
  where
    mkOpt Stage {long, short, description} =
      Opt.switch
        ( Opt.long long <> Opt.short short <> Opt.help description
        )
    dbgStd =
      Opt.flag'
        DbgStd
        ( Opt.long "std"
            <> Opt.help "Print debug messages to standard output"
        )
    dbgFile =
      Opt.strOption
        ( Opt.long "dbgf"
            <> Opt.metavar "FILE"
            <> Opt.help "Print debug messages in one file FILE"
        )
    dbgDir =
      Opt.strOption
        ( Opt.long "dbgd"
            <> Opt.metavar "DIR"
            <> Opt.help "Print debug messages in many files in dirrectory DIR"
        )

prefTabModeOption :: Opt.Parser PrefTabMode
prefTabModeOption =
  Opt.option
    reader
    ( Opt.long "prefTabMode"
        <> Opt.value ColumnTab
        <> Opt.showDefaultWith show'
        <> Opt.metavar "no|normal|column"
        <> Opt.help "How indent prefs content"
    )
  where
    reader =
      Opt.str >>= \case
        "no" -> return NoTab
        "normal" -> return NormalTab
        "column" -> return ColumnTab
        _ -> readerError "Accepted prefix tab modes are 'no', 'normal' and 'column'"
    show' = \case
      NoTab -> "no"
      NormalTab -> "normal"
      ColumnTab -> "column"

printOptions :: Opt.Parser PrintOpts
printOptions =
  PrintOpts
    <$> Opt.option
      Opt.auto
      ( Opt.long "tab"
          <> Opt.short 't'
          <> Opt.value 2
          <> Opt.showDefault
          <> Opt.metavar "TAB"
          <> Opt.help "Number of spaces for tabulation in output file"
      )
    <*> prefTabModeOption

options :: Opt.Parser Options
options =
  Options
    <$> Opt.strArgument
      ( Opt.metavar "SOURCE"
          <> Opt.help "Source file for translation"
      )
    <*> Opt.strOption
      ( Opt.long "out"
          <> Opt.short 'o'
          <> Opt.value ""
          <> Opt.showDefault
          <> Opt.metavar "FILE"
          <> Opt.help "Output file. If empty filename will be the same as SOURCE with .tex extension"
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "width"
          <> Opt.short 'w'
          <> Opt.value 100
          <> Opt.showDefault
          <> Opt.metavar "WIDTH"
          <> Opt.help "Number of charecters in a line in output file"
      )
    <*> printOptions
    <*> dbgOptions

main :: IO ()
main = processFile . defaultOutp =<< Opt.execParser opts
  where
    opts =
      Opt.info
        (Opt.helper <*> options)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate file"
            <> Opt.header ""
        )
    defaultOutp opts@Options {inpFile, outpFile}
      | null outpFile = opts {outpFile = replExt "tex" inpFile}
      | otherwise = opts
    replExt ext' filePath =
      case spanEnd (/= '.') filePath of
        ([], _) -> filePath <> "." <> ext'
        (f', _) -> f' <> ext'
