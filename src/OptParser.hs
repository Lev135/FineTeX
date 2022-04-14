{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module OptParser (
    OptParser (..), (<||>), (<??>), mkOptP, toParsec,
    flagP
) where
import Prelude hiding (fail)

import Control.Applicative
    ( Alternative(empty, (<|>), many), optional )
import Control.Monad.Fail (MonadFail (fail))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (Parsec, errorBundlePretty, parse,
    MonadParsec (eof, takeWhileP, lookAhead, parseError),
    SourcePos, getSourcePos,
    PosState(pstateSourcePos, pstateOffset),
    ParseErrorBundle (ParseErrorBundle, bundlePosState), region, ParseError (TrivialError), getOffset, ErrorItem (Label), manyTill)
import Data.Void (Void)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Char (isSpace)
import Utils (eitherFail)
import Control.Monad.State (StateT (StateT, runStateT))
import Control.Monad.Trans ( MonadTrans(lift) )
import Text.Megaparsec.Char (string, eol)
import Control.Monad.Except (MonadError (throwError))
import Data.Either.Extra (maybeToEither)
import Data.List ( intercalate )
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (fromList)
import Data.Maybe (isJust)
import Control.Monad (void)

type Parser = Parsec Void Text

type OptName = Text

data OptVal = OptVal {
        args        :: Text,
        nameOffset  :: Int,
        offset      :: Int
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
    opts <- flip manyTill (lookAhead $ void eol <|> eof) $ do
        nameOffset  <- getOffset
        name        <- optNameP
        offset      <- getOffset
        args        <- optArgsConsumer
        return (name, OptVal{nameOffset, offset, args})
    checkDistinct (fst <$> opts)
    (a, opts') <- either mkErr pure $ parseOptions optP opts
    case opts' of
        []                          -> return a
        (name, OptVal{nameOffset}) : _  -> let lbl = Label . fromList $ "option '" <> T.unpack name <> "'"
                    in parseError $ TrivialError nameOffset (Just lbl) mempty
    where
        mkErr Empty                          = fail   "empty"
        mkErr (IncorrectCombination opts)    = fail $ "Incorrect combination of options: " <> optsStr <> " cannot be used at once"
            where optsStr = intercalate ", " opts
        mkErr (NotFoundOption name)          = fail $ "Option not found: " <> T.unpack name
        mkErr (ArgParsingError n OptVal{offset} e) = parseError (h e offset)
            where
                h (ParseErrorBundle es s) offset = incOffset offset $ NE.head es

        incOffset pos (TrivialError p a b) = TrivialError (p + pos) a b
        incOffset _ _ = undefined

        checkDistinct [] = return ()
        checkDistinct (x : xs) | x `elem` xs = fail $ "Multiple option: " ++ show x
                               | otherwise   = checkDistinct xs

flagP :: Text -> OptParser Bool
flagP name = isJust <$> optional (mkOptP name (return ()))
