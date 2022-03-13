{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Generator where

import Prelude hiding (readFile)
import Utils ( sepBy_, failMsg, (.:), withError )
import Text.Megaparsec.Debug
import Text.Megaparsec(Parsec, MonadParsec (takeWhileP, label, takeWhile1P, try, notFollowedBy, lookAhead, eof), Pos, sepBy1, sepBy, unPos, (<?>), choice, optional, parse, errorBundlePretty, mkPos, satisfy, manyTill)
import Text.Megaparsec.Char ( char, space1, newline, letterChar, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (indentGuard)

import Data.Void(Void)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Monad (void, when, unless)
import Control.Applicative ( Alternative(empty, (<|>), some, many) )
import Data.Maybe (maybeToList, isJust, mapMaybe, fromMaybe)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Char (isLetter, isSpace, isAlphaNum)
import Data.List (intersperse)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), liftEither)
import Control.Monad.Catch (MonadCatch, catchIOError)
import Data.Text.IO (readFile)

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
pStringBetween ch = chP *> (T.pack <$> manyTill L.charLiteral chP)
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
        try $ do
            indentGuard sc EQ ind
            atLexeme name <* sc <* newline
        pa (incIndent ind)

inArgsEnvironment :: Text -> Parser args -> (args -> Pos -> Parser a) -> Pos -> Parser a
inArgsEnvironment name pargs pa ind
    = label ("Environment '" ++ unpack name ++ "'") $ do
        try $ do
            indentGuard sc EQ ind
            atLexeme name
        args <- pargs
        sc <* newline
        pa args (incIndent ind)

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
        sep = lookAhead (scn *> checkIndent) *> pEmptyL <* checkIndent <* notFollowedBy eof
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

data ArgType = ArgString
    deriving Show
type ArgName = Text

data Argument = Argument {
            atype :: ArgType,
            name  :: ArgName
        }
    deriving Show

newtype ArgV = ArgVString Text
    deriving Show

data Environment = Environment {
        name, begin, end    :: Text,
        args                :: [Argument],
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

data Definition
    = DefE  Environment
    | DefMC Command
    | DefC  Command
    | DefP  Pref
    deriving Show

pDefinitionBlock :: Parser [Definition]
pDefinitionBlock = skipImps zeroInd *> (fromMaybe [] <$> optional (pDefs zeroInd))
    where
        zeroInd = mkPos 1
        pDefs = inEnvironment "Define" $ concatIndentMany [] $ \ind ->
                map DefE  <$> pEnvsDef     ind
            <|> map DefC  <$> pCmdsDef     ind
            <|> map DefMC <$> pMathCmdsDef ind
            <|> map DefP  <$> pPrefDef     ind
        skipImps ind = inArgsEnvironment "Import" pStringLiteralL (const return) ind
             `sepBy` try (lookAhead (scn *> atLexeme "Import"))

pMathCmdsDef :: Pos -> Parser [Command]
pMathCmdsDef = inEnvironment "MathCommands"
    $ indentMany Nothing pCmdDef

pCmdsDef :: Pos -> Parser [Command]
pCmdsDef = inEnvironment "Commands"
    $ fail "Non-math commands are not realized yet. Use @MathCmds for commands in Math mode"

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

pType :: Parser ArgType
pType = ArgString <$ strLexeme "String"

pDefArgs :: Parser [Argument]
pDefArgs = many $ do
    try $ strLexeme "("
    name  <- pIdentifierL
    strLexeme ":"
    atype <- pType
    strLexeme ")"
    return Argument{name, atype}

pEnvsDef :: Pos -> Parser [Environment]
pEnvsDef = inEnvironment "Environments"
    $ indentMany Nothing $ noIndent $ do
        name         <- pIdentifierL
        args         <- pDefArgs
        strLexeme    "="
        (begin, end) <- pTeX <|> pBeginEnd
        innerMath <- isJust <$> optional (atLexeme "Math")
        newline
        return Environment{ name, begin, end, args, innerMath }
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
    | DocEnvironment   Environment [ArgV] [DocElement]
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


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("@" <> name) s

texDocElement :: Definitions -> Bool -> DocElement -> Text
texDocElement defs math (DocParagraph els)
        = T.unlines $ map (mconcat . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = repl begin <> texDocImpl defs (math || innerMath) els <> repl end
    where repl = replaceArgs args argvs
texDocElement _ _ (DocString s) = s

texParEl :: Definitions -> Bool -> ParEl -> Text
texParEl _    False (ParText    t) = t
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Text
texMath Definitions{mathCmds} = foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds


pDocument :: Definitions -> Parser [DocElement]
pDocument defs = L.nonIndented scn (pElements defs $ mkPos 1) <* scn

pElements :: Definitions -> Pos -> Parser [DocElement]
pElements defs = indentMany (Just $ DocString "\n") (pElement defs)

pElement :: Definitions -> Pos -> Parser DocElement
pElement defs ind
     =  pPrefLineEnvironment defs ind
    <|> pEnvironment         defs ind
    <|> pParagraph           defs ind

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
            DocEnvironment env [] <$> pElements defs ind''
    case group of
        Nothing         -> pEl
        Just groupName  -> do
            group <- lookup groupName envs `failMsg` "Unrecognized group environment " ++ unpack groupName
            els   <- pEl `sepBy` try pPref
            let els' = case sep of
                        Nothing -> els
                        Just s  -> intersperse (DocString s) els
            return $ DocEnvironment group [] els'

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

pArgV :: ArgType -> Parser ArgV
pArgV ArgString = ArgVString <$> pStringLiteralL

pEnvironment :: Definitions -> Pos -> Parser DocElement
pEnvironment defs@Definitions{envs} ind = do
    try $ indentGuard sc EQ ind *> string "@"
    name <- pIdentifierL
    env <- lookup name envs `failMsg` ("Undefined environment " ++ show name)
    args <- mapM pArgV (atype <$> args env)
    sc <* newline
    DocEnvironment env args <$> pElements defs (incIndent ind)

pFile :: Definitions -> Parser (Definitions, [DocElement])
pFile impDefs = do
    defs <- processDefs <$> pDefinitionBlock
    let defs' = impDefs <> defs
    doc  <- pDocument defs'
    return (defs', doc)

pImportFiles :: Parser [FilePath]
pImportFiles = L.nonIndented scn $
    try (atLexeme "Import" *> fmap unpack pStringLiteralL `sepBy` try (newline *> scn *> atLexeme "Import"))
        <|> return []

getImports :: FilePath -> Text -> Either String [FilePath]
getImports = first (("get imports error: " <>) . errorBundlePretty) .: parse pImportFiles

readDoc :: (MonadError String m, MonadIO m, MonadCatch m) => FilePath -> m (Definitions, [DocElement])
readDoc fileName = do
    file      <- liftIO (readFile fileName)
        `catchIOError` \e -> throwError ("Unable to open file '" <> fileName <> "': " <> show e)
    impFNames <- liftEither $ getImports fileName file
    defs      <- mconcat <$> mapM ((fst <$>) . withError addPrefix . readDoc) impFNames
    case parse (pFile defs) fileName file of
        Left   e   -> throwError $ errorBundlePretty e
        Right  res -> return res
    where
        addPrefix eStr = "While processing imports from " <> fileName <> ":\n" <> eStr
