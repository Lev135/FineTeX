{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.State (StateT (StateT, runStateT), MonadTrans (lift))

type Parser = Parsec Void Text

newtype OptParser a = OptParser {
        runOptParser :: StateT [(Text, Text)] (Either String) a
    }
    deriving (Functor, Applicative, Monad, Alternative)

instance MonadFail OptParser where
    fail e = OptParser $ lift (Left e)

parseOptions :: OptParser a -> [(Text, Text)] -> Either String (a, [(Text, Text)])
parseOptions pa = runStateT (runOptParser pa)  

parseText :: OptParser a -> Text -> Either String a
parseText p s = do
    let xs = second (T.dropWhile isSpace) . T.breakOn " " <$> tail (T.split (=='@') s)
    checkDistinct $ fst <$> xs
    (a, xs') <- parseOptions p xs
    checkEmpty xs'
    return a 
    where
        checkEmpty [] = Right ()
        checkEmpty ((n, _):_) = Left  $ "Unexpected option: " ++ show n
        checkDistinct [] = Right ()
        checkDistinct (x : xs) | x `elem` xs = Left $ "Multiple option: " ++ show x
                               | otherwise   = checkDistinct xs
-- StateT s m a -> s -> m a
-- | Strict alternative
(<||>) :: OptParser a -> OptParser a -> OptParser a
pa <||> pa' = OptParser $ StateT $ \xs -> 
    case (parseOptions pa xs, parseOptions pa' xs) of
        (Left _, ma) -> ma
        (ma, Left _) -> ma
        -- TODO : describe options
        _            -> Left "Incorrect combination of options"

mkOptP :: Text -> Parser a -> OptParser a
mkOptP name p = OptParser $ StateT $ \xs -> case lookup name xs of
    Nothing  -> Left $ "Option not found: " ++ T.unpack name
    -- TODO : correct position in inner parser
    Just txt -> case parse (p <* eof) "" txt of
        Left  e -> Left $ "Error while parsing " ++ T.unpack name ++ ": " ++ errorBundlePretty e
        Right a -> Right (a, filter ((/= name) . fst) xs)

pOpts :: OptParser a -> Parser a
pOpts p = do
    let getStr = takeWhileP Nothing (/= '\n')
    optStr <- lookAhead getStr
    a <- eitherFail $ parseText p optStr
    getStr
    return a

