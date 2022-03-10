module Main where

import Generator
import Data.Text(Text, pack, unpack)
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof), mkPos, Pos)
import System.Environment (getArgs)

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

parseFile :: FilePath -> IO ()
parseFile filePath = do
    inp <- pack <$> readFile filePath
    case parseAll (pFile mempty) inp of
      Left  e         -> putStrLn e
      Right (defs, r) -> putStrLn (unpack $ texDoc defs r)

processFile :: FilePath -> FilePath -> IO ()
processFile inpFile outpFile = do
    (defs, docEls) <- readDoc inpFile
    writeFile outpFile (unpack $ texDoc defs docEls)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inpF, outpF] -> processFile inpF outpF
        _             -> putStrLn "Incorrect number of arguments. Usage: texgen <inpFile> <outpFile>"
