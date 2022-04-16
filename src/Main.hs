module Main where

import Prelude hiding (writeFile)
import Generator
import Printing
import Data.Text(Text, pack, unpack)
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof), mkPos, Pos)
import System.Environment (getArgs)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Prettyprinter.Render.Text (renderStrict)
import Data.ByteString (writeFile)
import Prettyprinter (defaultLayoutOptions, layoutSmart, layoutPretty, LayoutOptions(..), PageWidth (AvailablePerLine))
import Data.Text.Encoding (encodeUtf8)
import qualified Options.Applicative as Opt
import Data.List.Extra (spanEnd)
import Control.Applicative (Alternative((<|>)))
import Options.Applicative (readerError)
import qualified IOUtils
import qualified Prettyprinter as P
import Data.Void (Void)
import qualified Data.Text as T 
import Data.Char (isSpace)

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

processFile :: Options -> IO ()
processFile Options{ inpFile, outpFile, pageWidth, printOpts } = do
    res <- runExceptT (readDoc inpFile :: ExceptT String IO (Definitions, [DocElement]))
    case res of
      Left   e             -> IOUtils.putStrLn e
      Right (defs, docEls) -> do
          let stream = layoutSmart renderOpts $ texDoc printOpts defs docEls
          writeFile outpFile $ encodeUtf8 . renderStrict . h $ stream
    where
        renderOpts = defaultLayoutOptions{layoutPageWidth = AvailablePerLine pageWidth 1.0}
        h :: P.SimpleDocStream Ann -> P.SimpleDocStream Void
        h sds = case sds of
            P.SFail                  -> P.SFail
            P.SEmpty                 -> P.SEmpty
            P.SChar c rest           -> P.SChar c   $ h rest
            P.SText l t rest         -> if noIndNext sds 
                                        then h rest 
                                        else P.SText l t $ h rest
            P.SLine i rest           -> if noIndNext sds 
                                        then P.SLine 0 $ h rest 
                                        else P.SLine i $ h rest
            P.SAnnPush NoIndent rest -> remInd rest
            P.SAnnPop rest           -> h rest
        remInd :: P.SimpleDocStream Ann -> P.SimpleDocStream Void
        remInd = \case
            P.SFail             -> P.SFail
            P.SEmpty            -> P.SEmpty
            P.SChar c rest      -> P.SChar c   $ remInd rest
            P.SText l t rest    -> P.SText l t $ remInd rest
            P.SLine i rest      -> P.SLine 0   $ remInd rest
            P.SAnnPush ann rest -> remInd rest
            P.SAnnPop rest      -> h rest
        noIndNext :: P.SimpleDocStream Ann -> Bool
        noIndNext = \case
            P.SAnnPush NoIndent _ -> True
            P.SLine _ rest -> noIndNext rest
            P.SText _ t rest -> T.all isSpace t && noIndNext rest
            _ -> False

data Options = Options {
        inpFile         :: FilePath,
        outpFile        :: FilePath,
        pageWidth       :: Int,
        printOpts       :: PrintOpts
    }

prefTabModeOption :: Opt.Parser PrefTabMode
prefTabModeOption = Opt.option reader
         ( Opt.long    "prefTabMode"
        <> Opt.value   ColumnTab
        <> Opt.showDefaultWith show'
        <> Opt.metavar "no|normal|column"
        <> Opt.help    "How indent prefs content"
        )
    where
        reader = Opt.str >>= \case
                "no"      -> return NoTab
                "normal"  -> return NormalTab
                "column"  -> return ColumnTab
                _         -> readerError "Accepted prefix tab modes are 'no', 'normal' and 'column'"
        show' = \case
            NoTab       -> "no"
            NormalTab   -> "normal"
            ColumnTab   -> "column"
printOptions :: Opt.Parser PrintOpts
printOptions = PrintOpts
    <$> Opt.option Opt.auto
         ( Opt.long     "tab"
        <> Opt.short    't'
        <> Opt.value    2
        <> Opt.showDefault
        <> Opt.metavar  "TAB"
        <> Opt.help     "Number of spaces for tabulation in output file"
        )
    <*> prefTabModeOption

options :: Opt.Parser Options
options = Options
    <$> Opt.strArgument
         ( Opt.metavar  "SOURCE"
        <> Opt.help     "Source file for translation"
        )
    <*> Opt.strOption
         ( Opt.long     "out"
        <> Opt.short    'o'
        <> Opt.value    ""
        <> Opt.showDefault
        <> Opt.metavar  "FILE"
        <> Opt.help     "Output file. If empty filename will be the same as SOURCE with .tex extension"
        )
    <*> Opt.option Opt.auto
         ( Opt.long     "width"
        <> Opt.short    'w'
        <> Opt.value    100
        <> Opt.showDefault
        <> Opt.metavar  "WIDTH"
        <> Opt.help     "Number of charecters in a line in output file"
        )
    <*> printOptions

main :: IO ()
main = processFile . defaultOutp =<< Opt.execParser opts
    where
        opts = Opt.info (Opt.helper <*> options)
             ( Opt.fullDesc
            <> Opt.progDesc     "Generate file"
            <> Opt.header       ""
            )
        defaultOutp opts@Options{inpFile, outpFile}
            | null outpFile = opts{outpFile = replExt "tex" inpFile}
            | otherwise     = opts
        replExt ext' filePath
                = case spanEnd (/= '.') filePath of
                    (_,     []) -> filePath <> ext'
                    (f',    _ ) -> f' <> ext'
