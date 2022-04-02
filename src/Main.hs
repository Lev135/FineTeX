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

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

options = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 100 1.0}

processFile :: FilePath -> FilePath -> IO ()
processFile inpFile outpFile = do
    res <- runExceptT (readDoc inpFile :: ExceptT String IO (Definitions, [DocElement]))
    case res of
      Left   e             -> putStrLn e
      Right (defs, docEls) -> do
          print $ head docEls 
          writeFile outpFile (encodeUtf8 . renderStrict . layoutSmart options $ texDoc defs docEls)

processFile' :: FilePath -> IO ()
processFile' f = processFile (f <> ".ttex") (f <> ".tex")

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inpF, outpF] -> processFile inpF outpF
        _             -> putStrLn "Incorrect number of arguments. Usage: texgen <inpFile> <outpFile>"
