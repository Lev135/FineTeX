{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OptParser
  ( OptParser (..),
    (<||>),
    (<??>),
    mkOptP,
    toParsec,
    labelOpt,
    flagOpt,
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    optional,
  )
import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.State (StateT (StateT, runStateT))
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.List (intercalate)
import Data.List.NonEmpty (fromList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( ErrorItem (Label),
    MonadParsec (eof, lookAhead, parseError),
    ParseError (TrivialError),
    ParseErrorBundle (ParseErrorBundle),
    Parsec,
    getOffset,
    manyTill,
    parse,
  )
import Text.Megaparsec.Char (eol)
import Prelude hiding (fail)

type Parser = Parsec Void Text

type OptName = Text

data OptVal = OptVal
  { args :: Text,
    nameOffset :: Int,
    offset :: Int
  }

data OptParserError
  = Empty
  | LabelError String
  | IncorrectCombination [String]
  | NotFoundOption OptName
  | ArgParsingError OptName OptVal (ParseErrorBundle Text Void)

type Opts = [(OptName, OptVal)]

newtype OptParser a = OptParser
  { runOptParser :: StateT Opts (Either OptParserError) a
  }
  deriving (Functor, Applicative, Monad, MonadError OptParserError)

withNonCriticalE :: OptParserError -> Either OptParserError a -> Either OptParserError a
withNonCriticalE e ma = case e of
  Empty -> ma
  LabelError {} -> Left e
  NotFoundOption {} -> ma
  IncorrectCombination {} -> Left e
  ArgParsingError {} -> Left e

instance Alternative OptParser where
  empty = throwError Empty
  pa <|> pb = OptParser $
    StateT $ \xs ->
      case parseOptions pa xs of
        Right a -> Right a
        Left e -> withNonCriticalE e $ parseOptions pb xs

instance MonadFail OptParser where
  fail = OptParser . StateT . const . Left . LabelError

parseOptions :: OptParser a -> Opts -> Either OptParserError (a, Opts)
parseOptions pa = runStateT (runOptParser pa)

-- StateT s m a -> s -> m a

-- | Strict alternative
(<||>) :: OptParser a -> OptParser a -> OptParser a
pa <||> pa' = OptParser $
  StateT $ \xs ->
    case (parseOptions pa xs, parseOptions pa' xs) of
      (Left e, ma) -> withNonCriticalE e ma
      (ma, Left _) -> ma
      -- TODO : describe options
      _ -> Left (IncorrectCombination [])

-- | Comments to strict alternative (<||>)
(<??>) :: OptParser a -> [String] -> OptParser a
pa <??> opts = OptParser $
  StateT $ \xs ->
    case parseOptions pa xs of
      Left (IncorrectCombination opts') ->
        Left $ IncorrectCombination $ opts' <> opts
      ma -> ma

mkOptP :: Text -> Parser a -> OptParser a
mkOptP name p = OptParser $
  StateT $ \opts -> do
    optVal <- maybeToEither (NotFoundOption name) $ lookup name opts
    a <- first (ArgParsingError name optVal) $ parse (p <* eof) "" (args optVal)
    return (a, filter ((/= name) . fst) opts)

toParsec :: Parser OptName -> Parser Text -> OptParser a -> Parser a
toParsec optNameP optArgsConsumer optP = do
  opts <- flip manyTill (lookAhead $ void eol <|> eof) $ do
    nameOffset <- getOffset
    name <- optNameP
    offset <- getOffset
    args <- optArgsConsumer
    return (name, OptVal {nameOffset, offset, args})
  checkDistinct (fst <$> opts)
  (a, opts') <- either mkErr pure $ parseOptions optP opts
  case opts' of
    [] -> return a
    (name, OptVal {nameOffset}) : _ ->
      let lbl = Label . fromList $ "option '" <> T.unpack name <> "'"
       in parseError $ TrivialError nameOffset (Just lbl) mempty
  where
    mkErr Empty = fail "empty"
    mkErr (LabelError err) = fail err
    mkErr (IncorrectCombination opts) = fail $ "Incorrect combination of options: " <> optsStr <> " cannot be used at once"
      where
        optsStr = intercalate ", " opts
    mkErr (NotFoundOption name) = fail $ "Option not found: " <> T.unpack name
    mkErr (ArgParsingError _ OptVal {offset} e) = parseError (h e offset)
      where
        h (ParseErrorBundle es _) offset = incOffset offset $ NE.head es

    incOffset pos (TrivialError p a b) = TrivialError (p + pos) a b
    incOffset _ _ = undefined

    checkDistinct [] = return ()
    checkDistinct (x : xs)
      | x `elem` xs = fail $ "Multiple option: " ++ show x
      | otherwise = checkDistinct xs

labelOpt :: Text -> OptParser ()
labelOpt name = mkOptP name (return ())

flagOpt :: Text -> OptParser Bool
flagOpt name = isJust <$> optional (labelOpt name)
