module Main where

import Generator
import Data.Text(Text, pack, unpack)
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof), mkPos, Pos)

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

parseFile :: FilePath -> IO ()
parseFile filePath = do
    inp <- pack <$> readFile filePath
    case parseAll pFile inp of
      Left  e         -> putStrLn e
      Right (defs, r) -> putStrLn (unpack $ texDoc defs r)

processFile :: FilePath -> FilePath -> IO ()
processFile inpFile outpFile = do
    inp <- pack <$> readFile inpFile
    case parseAll pFile inp of
      Left  e -> putStrLn e
      Right (defs, r) -> writeFile outpFile (unpack $ texDoc defs r)


printErr :: Show a => Either String a -> IO ()
printErr (Left  e) = putStrLn e
printErr (Right v) = print v

test :: Show a => (Definitions -> Pos -> Parser a) -> Int -> Text -> IO ()
test p n str= do
    inp <- pack <$> readFile "tests/defs2.ttex"
    case parseAll pDefinitionBlock inp of
        Left  e   -> putStrLn e
        Right des -> printErr $ parseAll (p (processDefs des) (mkPos n)) str

main = undefined
