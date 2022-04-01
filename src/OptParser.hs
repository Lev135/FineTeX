{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OptParser (
    OptParser (..), (<||>), (<??>), mkOptP, toParsec
) where
import Prelude hiding (fail)

import Control.Applicative (Alternative (empty, (<|>), many))
import Control.Monad.Fail (MonadFail (fail))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (Parsec, errorBundlePretty, parse, MonadParsec (eof, takeWhileP, lookAhead), SourcePos, getSourcePos, ParseErrorBundle)
import Data.Void (Void)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Char (isSpace)
import Utils (eitherFail)
import Control.Monad.State (StateT (StateT, runStateT))
import Control.Monad.Trans ( MonadTrans(lift) ) 
import Text.Megaparsec.Char (string)
import Control.Monad.Except (MonadError (throwError))
import Data.Either.Extra (maybeToEither)
import Data.List ( intercalate )

type Parser = Parsec Void Text

type OptName = Text

data OptVal = OptVal {
        args  :: Text,
        pos   :: SourcePos 
    }

data OptParserError
    = Empty
    | IncorrectCombination [String]
    | NotFoundOption OptName
    | ArgParsingError OptName OptVal (ParseErrorBundle Text Void)

type Opts = [(OptName, OptVal)]

newtype OptParser a = OptParser {
        runOptParser :: StateT Opts (Either OptParserError) a
    }
    deriving (Functor, Applicative, Monad, MonadError OptParserError)

withNonCriticalE :: OptParserError -> Either OptParserError a -> Either OptParserError a
withNonCriticalE e ma = case e of 
    Empty                   -> ma
    NotFoundOption {}       -> ma
    IncorrectCombination{}  -> Left e
    ArgParsingError {}      -> Left e

instance Alternative OptParser where
    empty = throwError Empty
    pa <|> pb = OptParser $ StateT $ \xs ->
        case parseOptions pa xs of
            Right a -> Right a
            Left  e -> withNonCriticalE e $ parseOptions pb xs  

parseOptions :: OptParser a -> Opts -> Either OptParserError (a, Opts)
parseOptions pa = runStateT (runOptParser pa)  

-- StateT s m a -> s -> m a
-- | Strict alternative
(<||>) :: OptParser a -> OptParser a -> OptParser a
pa <||> pa' = OptParser $ StateT $ \xs -> 
    case (parseOptions pa xs, parseOptions pa' xs) of
        (Left e, ma) -> withNonCriticalE e ma
        (ma, Left _) -> ma
        -- TODO : describe options
        _            -> Left (IncorrectCombination [])

-- | Comments to strict alternative (<||>)
(<??>) :: OptParser a -> [String] -> OptParser a
pa <??> opts = OptParser $ StateT $ \xs ->
    case parseOptions pa xs of
        Left  (IncorrectCombination opts') 
            -> Left $ IncorrectCombination $ opts' <> opts
        ma  -> ma

mkOptP :: Text -> Parser a -> OptParser a
mkOptP name p = OptParser $ StateT $ \opts -> do
    optVal <- maybeToEither (NotFoundOption name) $ lookup name opts
    a      <- first (ArgParsingError name optVal) $ parse (p <* eof) "" (args optVal)
    return (a, filter ((/= name) . fst) opts)

toParsec :: Parser OptName -> Parser Text -> OptParser a -> Parser a
toParsec optNameP optArgsConsumer optP = do
    opts <- many $ do
        name <- optNameP
        pos  <- getSourcePos
        args <- optArgsConsumer
        return (name, OptVal{pos, args})
    checkDistinct (fst <$> opts)
    (a, opts') <- either (fail . mkErrMsg) pure $ parseOptions optP opts
    case opts' of
        []            -> return a
        (name, _) : _ -> fail $ "Unknown option: " ++ show name
    where
        mkErrMsg :: OptParserError -> String
        mkErrMsg Empty                          = "empty"
        mkErrMsg (IncorrectCombination opts)    = "Incorrect combination of options: " <> optsStr <> " cannot be used at once"
            where optsStr = intercalate ", " opts 
        mkErrMsg (NotFoundOption name)          = "Option not found: " <> T.unpack name
        mkErrMsg (ArgParsingError n OptVal{args, pos} e) = "Error while parsing arguments: " <> errorBundlePretty e
        
        checkDistinct [] = return ()
        checkDistinct (x : xs) | x `elem` xs = fail $ "Multiple option: " ++ show x
                               | otherwise   = checkDistinct xs


