{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module FineTeX.Parser.Utils where

import Control.Applicative (Alternative)
import Control.Monad (void)
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.Function (on)
import Data.List.NonEmpty (fromList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.Utils (Box (..), Pos, PosC (..), failMsg, foldMapBox, sequenceBox)
import GHC.Stack (HasCallStack)
import Text.Megaparsec
  ( ErrorItem (Label),
    MonadParsec (eof, lookAhead, takeWhile1P, takeWhileP),
    anySingle,
    choice,
    empty,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    manyTill_,
    optional,
    region,
    satisfy,
    sepBy,
    setErrorOffset,
    some,
    unexpected,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Primitives

type ParserM m = (MonadParsec Void Text m, MonadFail m)

data Posed a = Posed {val :: a, pos :: Pos}

instance Eq a => Eq (Posed a) where
  (==) = (==) `on` unBox

instance Ord a => Ord (Posed a) where
  (<=) = (<=) `on` unBox

instance Show a => Show (Posed a) where
  show Posed {val} = show val -- show (b, e) <> ": " <> show val

instance Box Posed where
  unBox = val

instance PosC Posed where
  getPos = pos

instance Functor Posed where
  fmap f Posed {val, pos} = Posed {val = f val, pos = pos}

instance Foldable Posed where
  foldMap = foldMapBox

instance Traversable Posed where
  sequenceA = sequenceBox

withPos :: ParserM m => m a -> m (Posed a)
withPos pa = do
  b <- getSourcePos
  a <- pa
  e <- getSourcePos
  return $ Posed a (b, e)

-- | parse at least one space or tab symbol
sp1 :: ParserM m => m ()
sp1 = void $ some (char ' ' <|> char '\t')

-- | parse spaces or tab symbols
sp :: ParserM m => m ()
sp = void $ many (char ' ' <|> char '\t')

-- | 'eol' or 'eof'
eolf :: ParserM m => m ()
eolf = void eol <|> eof

-- | parse line comment starting with '%'
--   'eol' after comment will be not consumed
lineComment :: ParserM m => m ()
lineComment = void $ char '%' *> manyTill anySingle (lookAhead eolf)

-- | line spaces or comments \\
--   'eol' after comment will be not consumed
sc :: ParserM m => m ()
sc = L.space sp1 lineComment empty

-- | line spaces, comments or 'eol'
scn :: ParserM m => m ()
scn = void $ sc `sepBy` eol

-- | line spaces, comments and at least one 'eol'
scn1 :: ParserM m => m ()
scn1 = sc *> eol *> scn

-- | Lexeme with spaces and 'eol' or comments after it
lexeme :: ParserM m => m a -> m (Posed a)
lexeme = withPos . L.lexeme sc

-- | String with spaces or comments after it
strLexeme :: ParserM m => Text -> m (Posed Text)
strLexeme = lexeme . string

-- | String lexeme with `@` before it
atLexeme :: ParserM m => Text -> m (Posed Text)
atLexeme = strLexeme . ("@" <>)

-- | Parse string literal between `borderCh`
pStringBetween :: ParserM m => Char -> m Text
pStringBetween borderCh = do
  char borderCh
  (str, isEol) <- manyTill_ anySingle (False <$ char borderCh <|> True <$ lookAhead eol)
  if isEol
    then unexpected . Label . fromList $ "end of line"
    else return $ T.pack str

-- | String literal lexeme between " and ' symbols
pStringLiteralL :: ParserM m => m (Posed Text)
pStringLiteralL =
  lexeme (choice (pStringBetween <$> ['"', '\'']))
    <?> "String literal"

pIdentifierL :: ParserM m => m (Posed Text)
pIdentifierL =
  lexeme
    ( T.cons
        <$> satisfy isLetter
        <*> takeWhile1P Nothing (\ch -> isAlphaNum ch || ch `elem` ['-', '\''])
    )
    <?> "Identifier"

pPrefix :: ParserM m => m Text
pPrefix =
  T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing isWordCh
    <?> "Prefix"
  where
    fstSmbls :: [Char]
    fstSmbls = "!#$%^*-+,./|><[]~"

pPrefixL :: ParserM m => m (Posed Text)
pPrefixL = lexeme pPrefix

pWordL :: ParserM m => m (Posed Text)
pWordL = lexeme (takeWhile1P Nothing isWordCh) <?> "Word"

isWordCh :: Char -> Bool
isWordCh ch = not $ isSpace ch || ch == '%'

parseMapEl :: (Box p, Ord k, Show k, ParserM m) => Map k a -> String -> m (p k) -> m (p k, a)
parseMapEl m kType pk = do
  reg <- region . setErrorOffset <$> getOffset
  k <- pk
  reg $ (k,) <$> (M.lookup (unBox k) m `failMsg` "Undefined " <> kType <> " " <> show k)

assert :: HasCallStack => (Monad m, Alternative m) => m a -> m ()
assert p = do
  b <- optional p
  case b of
    Nothing -> error "Assertion failed"
    Just _ -> return ()
