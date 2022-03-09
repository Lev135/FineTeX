{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Generator where

import Utils

import Text.Megaparsec(Parsec, MonadParsec (takeWhileP, label, takeWhile1P, try, notFollowedBy, lookAhead, eof), Pos, sepBy1, sepBy, unPos, (<?>), choice, optional, parse, errorBundlePretty, mkPos, satisfy)
import Text.Megaparsec.Char ( char, space1, newline, letterChar, string )
import Text.Megaparsec.Debug ()
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void(Void)
import Data.Text (Text, intercalate, replace, pack, unpack)
import qualified Data.Text as T
import Control.Monad (void, when, unless)
import Control.Applicative ( Alternative(empty, (<|>), some, many) )
import Text.Megaparsec.Char.Lexer (indentGuard)
import Data.Maybe (maybeToList, isJust, mapMaybe)
import Data.Bifunctor (Bifunctor(second))
import Data.Char (isLetter, isSpace, isAlphaNum)
import Data.List (intersperse)

dbg = flip const

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void . some $ char ' ' <|> char '\t') empty empty

scn :: Parser ()
scn = L.space space1 empty empty

indentLevel :: Parser Pos
indentLevel = sc *> L.indentLevel

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

strLexeme :: Text -> Parser Text
strLexeme = lexeme . string

atLexeme :: Text -> Parser Text
atLexeme = strLexeme . ("@" <>)

pStringBetween :: Char -> Parser Text
pStringBetween ch = chP *> takeWhileP Nothing (/= ch) <* chP
    where chP = string $ T.singleton ch

pStringLiteralL :: Parser Text
pStringLiteralL = lexeme (choice (pStringBetween <$> ['"', '\'']))
    <?> "String literal"

pIdentifierL :: Parser Text
pIdentifierL = lexeme (takeWhile1P Nothing (\ch -> isLetter ch || ch == '-'))
    <?> "Identifier"

pOperator :: Parser Text
pOperator = T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing (not . isSpace)
    <?> "Operator"
    where
        fstSmbls :: [Char]
        fstSmbls = "!@#$%^&*-+,./|\\><{}[]~"

pOperatorL :: Parser Text
pOperatorL = lexeme pOperator

pCommandL :: Parser Text
pCommandL = pOperatorL <|> pIdentifierL
    <?> "Command"

tabWidth :: Int
tabWidth = 2

incIndent :: Pos -> Pos
incIndent = mkPos . (+ tabWidth) . unPos

inEnvironment :: Text -> (Pos -> Parser a) -> Pos -> Parser a
inEnvironment name pa ind
    = label ("Environment '" ++ unpack name ++ "'") $ do
        try $ indentGuard sc EQ ind
        try $ atLexeme name
        sc <* newline
        pa (incIndent ind)

inPref :: Text -> (Pos -> Parser a) -> Pos -> Parser a
inPref name pa ind
    = label ("Pref '" ++ unpack name ++ "'") $ do
        try $ indentGuard sc GT ind
        try $ string $ name <> " "
        ind' <- indentLevel
        pa ind'

indentMany :: Maybe a -> (Pos -> Parser a) -> Pos -> Parser [a]
indentMany emptyL pa ind = pa ind `sepBy_` try sep
    where
        sep = checkIndent *> pEmptyL <* checkIndent <* notFollowedBy eof
        checkIndent = do
            ind' <- indentLevel
            unless (ind' >= ind)
                $ fail $ "Incorrect indentation (got "
                    <> show (unPos ind')
                    <> " should be greater or equal "
                    <> show (unPos ind)
        pEmptyL     = (emptyL <*) <$> (optional . try $ sc <* newline <* scn)

concatIndentMany :: [a] -> (Pos -> Parser [a]) -> Pos -> Parser [a]
concatIndentMany emptyL pa ind = concat <$> indentMany (Just emptyL) pa ind

noIndent :: Parser a -> Pos -> Parser a
noIndent a ind = indentGuard sc EQ ind >> a

data Environment = Environment {
        name, begin, end    :: Text,
        innerMath           :: Bool
    }
    deriving Show
data Pref = Pref {
        name        :: Text,
        env         :: Text,
        group       :: Maybe Text,
        sep         :: Maybe Text
    }
    deriving Show
data Command  = Command {
        name, val   :: Text
    }
    deriving Show

-- instance Show Environment where
--     show Environment{name} = unpack name

data Definition
    = DefE  Environment
    | DefMC Command
    | DefC  Command
    | DefP  Pref
    deriving Show

pDefinitionBlock :: Parser [Definition]
pDefinitionBlock = flip (inEnvironment "Define") (mkPos 1)
    $ concatIndentMany [] $ \ind ->
                map DefE  <$> pEnvsDef     ind
            <|> map DefC  <$> pCmdsDef     ind
            <|> map DefMC <$> pMathCmdsDef ind
            <|> map DefP  <$> pPrefDef     ind

pMathCmdsDef :: Pos -> Parser [Command]
pMathCmdsDef = inEnvironment "MathCommands"
    $ indentMany Nothing pCmdDef

pCmdsDef :: Pos -> Parser [Command]
pCmdsDef = inEnvironment "Commands"
    $ indentMany Nothing pCmdDef

pCmdDef :: Pos -> Parser Command
pCmdDef = noIndent $ do
    name      <- pCommandL
    strLexeme "="
    val       <- pStringLiteralL
    math      <- isJust <$> optional (atLexeme "Math")
    newline
    return Command{ name, val }

permute2 :: Alternative m => m a -> m b -> m (a, b)
permute2 a b = choice [
        (,) <$> a <*> b,
        (\b a -> (a, b)) <$> b <*> a
    ]

pEnvsDef :: Pos -> Parser [Environment]
pEnvsDef = inEnvironment "Environments"
    $ indentMany Nothing $ noIndent $ do
        name         <- pIdentifierL
        strLexeme    "="
        (begin, end) <- pTeX <|> pBeginEnd
        innerMath <- isJust <$> optional (atLexeme "Math")
        newline
        return Environment{ name, begin, end, innerMath }
    where
        pTeX = do
            atLexeme "TeX"
            texI <- pStringLiteralL
            return ("\\begin{" <> texI <> "}\n", "\\end{"   <> texI <> "}\n")
        pBeginEnd = (,) <$> pStringLiteralL <*> pStringLiteralL

pPrefDef :: Pos -> Parser [Pref]
pPrefDef = inEnvironment "Prefs"
    $ indentMany Nothing $ noIndent $ do
        name      <- pOperatorL
        strLexeme "="
        env       <- pIdentifierL
        (group, sep) <- permute2
                (optional $ atLexeme "Group" *> pIdentifierL   )
                (optional $ atLexeme "Sep"   *> pStringLiteralL)
        newline
        return Pref{name, env, group, sep}

data Definitions = Definitions {
        envs         :: [(Text, Environment)],
        cmds         :: [(Text, Command)],
        mathCmds     :: [(Text, Command)],
        prefs        :: [(Text, Pref)]
    }
    deriving Show

instance Semigroup Definitions where
    (Definitions a b c d) <> (Definitions a' b' c' d')
        = Definitions (a <> a') (b <> b') (c <> c') (d <> d')
instance Monoid Definitions where
    mempty = Definitions [] [] [] []

processDefs :: [Definition] -> Definitions
processDefs = mconcat . map processDef

processDef :: Definition -> Definitions
processDef (DefE env@Environment{name, begin, end, innerMath})
    = mempty { envs     = [(name, env)] }
processDef (DefC cmd@Command{name})
    = mempty { cmds     = [(name, cmd)] }
processDef (DefMC cmd@Command{name})
    = mempty { mathCmds = [(name, cmd)] }
processDef (DefP pr@Pref{name})
    = mempty {prefs     = [(name, pr)]}

data DocElement
    = DocParagraph     [[ParEl]]
    | DocEnvironment   Environment [DocElement]
    | DocString        Text
    deriving Show

data ParEl
    = ParText Text
    | ParFormula Text
    deriving Show

texDoc :: Definitions -> [DocElement] -> Text
texDoc = flip texDocImpl False

texDocImpl :: Definitions -> Bool -> [DocElement] -> Text
texDocImpl defs math = T.concat . map (texDocElement defs math)

texDocElement :: Definitions -> Bool -> DocElement -> Text
texDocElement defs math (DocParagraph els)
        = T.unlines $ map (mconcat . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, innerMath} els)
        = begin <> texDocImpl defs (math || innerMath) els <> end
texDocElement _ _ (DocString s) = s

texParEl :: Definitions -> Bool -> ParEl -> Text
texParEl _    False (ParText    t) = t
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Text
texMath Definitions{mathCmds} = foldr (.) id fs
    where fs = map (uncurry replace . second ((<>" ") . val)) mathCmds


pDocument :: Definitions -> Parser [DocElement]
pDocument defs = L.nonIndented scn (pElements defs $ mkPos 1) <* scn

pElements :: Definitions -> Pos -> Parser [DocElement]
pElements defs = indentMany (Just $ DocString "\n") (pElement defs)

pElement :: Definitions -> Pos -> Parser DocElement
pElement defs ind
     =  dbg "pref" (pPrefLineEnvironment defs ind)
    <|> dbg "env"  (pEnvironment         defs ind)
    <|> dbg "par"  (pParagraph           defs ind)

pPrefLineEnvironment :: Definitions -> Pos -> Parser DocElement
pPrefLineEnvironment defs@Definitions{prefs, envs} ind = do
    try $ indentGuard sc GT ind
    ind'     <- indentLevel
    name     <- pOperator <* string " "
    Pref{env = envName, group, sep} <-
        lookup name prefs `failMsg` "Unexpected prefix: " ++ unpack name
    env      <- lookup envName envs `failMsg` "Unrecognized environment " ++ unpack envName
    let pPref = indentGuard sc EQ ind' *> string (name <> " ")
        pEl = do
            ind'' <- indentLevel
            DocEnvironment env <$> dbg "prefEnv inner els" (pElements defs ind'')
    case group of
        Nothing         -> pEl
        Just groupName  -> do
            group <- lookup groupName envs `failMsg` "Unrecognized group environment " ++ unpack groupName
            els   <- pEl `sepBy` try pPref
            let els' = case sep of
                        Nothing -> els
                        Just s  -> intersperse (DocString s) els
            return $ DocEnvironment group els'

pParagraph :: Definitions -> Pos -> Parser DocElement
pParagraph defs ind = do
    indentGuard sc EQ ind
    DocParagraph <$> (pParLine `sepBy1` try sep) <* newline
    where
        sep = newline >> indentGuard sc EQ ind >> notFollowedBy (void newline <|> eof)

pParLine :: Parser [ParEl]
pParLine = some (pText <|> pForm)
    where
        pText = ParText <$> takeWhile1P Nothing smbl
              <?> "Paragraph text"
        pForm = ParFormula
              <$> (char '`' *> takeWhile1P Nothing smbl <* char '`')
              <?> "Inline formula"
        smbl = (`notElem` ['`', '\n'])

pEnvironment :: Definitions -> Pos -> Parser DocElement
pEnvironment defs@Definitions{envs} ind = do
    name <- try $ do
        indentGuard sc EQ ind
        string "@" *> pIdentifierL <* sc <* newline
    case lookup name envs of
        Nothing  -> fail $ "Undefined environment " ++ show name
        Just env -> DocEnvironment env <$> pElements defs (incIndent ind)

pFile :: Parser (Definitions, [DocElement])
pFile = do
    defs <- processDefs <$> pDefinitionBlock
    doc  <- pDocument defs
    return (defs, doc)
