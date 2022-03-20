{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module OptParser (
    OptParser (..), (<||>), mkOptP, pOpts
) where
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Fail (MonadFail (fail))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (Parsec, errorBundlePretty, parse, MonadParsec (eof, takeWhileP, lookAhead))
import Data.Void (Void)
import Data.Bifunctor (Bifunctor(second))
import Data.Char (isSpace)
import Utils (eitherFail)

type Parser = Parsec Void Text

newtype OptParser a = OptParser {
        runOptParser :: [(Text, Text)] -> Either String ([(Text, Text)], a)
    }
instance Functor OptParser where
    fmap f = OptParser . fmap (fmap (second f)) . runOptParser

instance Applicative OptParser where
    pure a    = OptParser $ Right . (, a)
    pf <*> pa = OptParser $ \xs -> do
        (xs',  f) <- runOptParser pf xs
        (xs'', a) <- runOptParser pa xs'
        pure (xs'', f a)

instance Monad OptParser where
    pa >>= k = OptParser $ \xs -> do
        (xs', a) <- runOptParser pa xs
        runOptParser (k a) xs'

instance Alternative OptParser where
    empty = OptParser $ \xs -> Left "empty"
    pa <|> pa' = OptParser $ \xs ->
        runOptParser pa xs <|> runOptParser pa' xs

instance MonadFail OptParser where
    fail e = OptParser $ const (Left e)


parseText :: OptParser a -> Text -> Either String a
parseText p s = do
    let xs = second (T.dropWhile isSpace) . T.breakOn " " <$> tail (T.split (=='@') s)
    checkDistinct $ fst <$> xs
    h =<< runOptParser p xs
    where
        filterEmpty = filter $ \name -> T.all isSpace name
        h ([],  a) = Right a
        h ((n, _):_, a) = Left  $ "Unexpected option: " ++ show n
        checkDistinct [] = Right ()
        checkDistinct (x : xs) | x `elem` xs = Left $ "Multiple option: " ++ show x
                               | otherwise   = checkDistinct xs

-- | Strict alternative
(<||>) :: OptParser a -> OptParser a -> OptParser a
pa <||> pa' = OptParser $ \xs -> do
    case (runOptParser pa xs, runOptParser pa' xs) of
        (Left _, ma) -> ma
        (ma, Left _) -> ma
        -- TODO : describe options
        _            -> Left "Incorrect combination of options"

mkOptP :: Text -> Parser a -> OptParser a
mkOptP name p = OptParser $ \xs -> case lookup name xs of
    Nothing  -> Left $ "Option not found: " ++ T.unpack name
    -- TODO : correct position in inner parser
    Just txt -> case parse (p <* eof) "" txt of
        Left  e -> Left $ "Error while parsing " ++ T.unpack name ++ ": " ++ errorBundlePretty e
        Right a -> Right (filter ((/= name) . fst) xs, a)

pOpts :: OptParser a -> Parser a
pOpts p = do
    let getStr = takeWhileP Nothing (/= '\n')
    optStr <- lookAhead getStr
    a <- eitherFail $ parseText p optStr
    getStr
    return a

