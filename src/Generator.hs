module Generator where

import Prelude hiding (readFile, fail)
import Utils ( sepBy_, failMsg, (.:), withError, eitherFail )
import OptParser ( OptParser, (<||>), mkOptP, pOpts )
import Text.Megaparsec.Debug
import Text.Megaparsec(Parsec, MonadParsec (takeWhileP, label, takeWhile1P, try, notFollowedBy, lookAhead, eof), Pos, sepBy1, sepBy, unPos, (<?>), choice, optional, parse, errorBundlePretty, mkPos, satisfy, manyTill, option)
import Text.Megaparsec.Char ( char, space1, newline, letterChar, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (indentGuard)

import Data.Void(Void)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Monad (void, when, unless, join, guard, forM)
import Control.Applicative ( Alternative(empty, (<|>), some, many) )
import Data.Maybe (maybeToList, isJust, mapMaybe, fromMaybe)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Char (isLetter, isSpace, isAlphaNum)
import Data.List (intersperse)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), liftEither)
import Control.Monad.Catch (MonadCatch, catchIOError)
import Data.Text.IO (readFile)
import Control.Monad.Fail (MonadFail (fail))

-- Primitives

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
        fstSmbls = "!#$%^&*-+,./|\\><[]~"


pPrefix :: Parser Text
pPrefix = T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing (\ch -> not $ isSpace ch || isAlphaNum ch)
    <?> "Prefix"
    where
        fstSmbls :: [Char]
        fstSmbls = "!#$%^*-+,./|\\><[]~"

pPrefixL :: Parser Text
pPrefixL = lexeme pPrefix

pOperatorL :: Parser Text
pOperatorL = lexeme pOperator

pCommandL :: Parser Text
pCommandL = pOperatorL <|> pIdentifierL
    <?> "Command"

inEnvironment :: Text -> Maybe el -> ([el] -> a) -> Parser el -> Parser a
inEnvironment name emptyEl f
    = inArgsEnvironment name emptyEl (return ()) (const f)

block :: Int -> Maybe el -> ([el] -> a) -> Parser el -> Parser a
block ind emptyEl f pel = do
    let checkInd = do
            ind' <- indentLevel
            guard (unPos ind' > ind)
                `failMsg` "Incorrect indentation (should be greater than " <> show ind <> ")"
        emptyL   = emptyEl <$ sc <* newline <* scn

    f <$> choice [
            try checkInd >> pel `sepBy_` try ((join <$> optional (try emptyL)) <* checkInd),
            pure []
        ]

inArgsEnvironment :: Text -> Maybe el -> Parser args -> (args -> [el] -> a) -> Parser el -> Parser a
inArgsEnvironment name emptyEl pargs f pel
    = label ("Environment " ++ show name) $ do
        ind <- try $ do
            ind <- indentLevel
            atLexeme name <* sc
            return ind
        args <- sc *> pargs <* sc <* newline
        block (unPos ind) emptyEl (f args) pel

-- Definition block

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
        name                :: Text,
        begin, end          :: Maybe Text,
        args                :: [Argument],
        innerMath           :: Bool
    }
    deriving Show
data Pref = Pref {
        name        :: Text,
        begin, end  :: Maybe Text,
        pref,  sep  :: Maybe Text,
        innerMath   :: Bool
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
pDefinitionBlock = skipImps *> scn *> (fromMaybe [] <$> optional pDefs)
    where
        pDefs = inEnvironment "Define" Nothing concat pDef
        pDef  = map DefE  <$> pEnvsDef
            <|> map DefMC <$> pMathCmdsDef
            <|> map DefP  <$> pPrefDef
        skipImps = inArgsEnvironment "Import" Nothing pStringLiteralL (const $ const ()) (return ())
             `sepBy` try (lookAhead (scn *> atLexeme "Import"))

pMathCmdsDef :: Parser [Command]
pMathCmdsDef = inEnvironment "MathCommands" Nothing id $ do
    name      <- pCommandL
    strLexeme "="
    val       <- pStringLiteralL
    newline
    return Command{ name, val }

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

pBeginEndOpt :: OptParser (Maybe Text, Maybe Text)
pBeginEndOpt = option (Nothing, Nothing) $ texBEP <||> simpleBEP
    where
        beginEndP = texBEP <||> simpleBEP
        texBEP = do
            texName <- mkOptP "TexBeginEnd" pStringLiteralL
            return (Just $ "\\begin{" <> texName <> "}", Just $ "\\end{"   <> texName <> "}")
        simpleBEP = do
            begin <- optional $ mkOptP "Begin" pStringLiteralL
            end   <- optional $ mkOptP "End" pStringLiteralL
            case (begin, end) of
                (Nothing, Nothing) -> fail "No @Begin @End"
                _ -> return (begin, end)

pEnvsDef :: Parser [Environment]
pEnvsDef = inEnvironment "Environments" Nothing id $ do
        name         <- pIdentifierL
        args         <- pDefArgs
        strLexeme    "="
        ((begin, end), innerMath) <- pOpts $ (,) <$> pBeginEndOpt <*> mathP
        newline
        return Environment{ name, begin, end, args, innerMath }
    where
        mathP = isJust <$> optional (mkOptP "Math" (return ()))

pPrefDef :: Parser [Pref]
pPrefDef = inEnvironment "Prefs" Nothing id $ do
        name      <- pPrefixL
        strLexeme "="
        ((begin, end), pref, sep, innerMath) <- pOpts
                    $ (,,,) <$> pBeginEndOpt <*> prefP <*> sepP <*> mathP
        newline
        return Pref{name, begin, end, pref, sep, innerMath}
    where
        prefP = optional (mkOptP "Pref" pStringLiteralL)
        sepP  = optional (mkOptP "Sep"  pStringLiteralL)
        mathP = isJust <$> optional (mkOptP "Math" (return ()))

-- Processing definitions

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

-- Document body

data DocElement
    = DocParagraph     [[ParEl]]
    | DocEnvironment   Environment [ArgV] [DocElement]
    | DocPrefGroup     Pref [[DocElement]]
    | DocEmptyLine
    deriving Show

data ParEl
    = ParText Text
    | ParFormula Text
    deriving Show

pDocument :: Definitions -> Parser [DocElement]
pDocument defs = scn *> pElements 0 defs <* scn

pElements :: Int -> Definitions -> Parser [DocElement]
pElements ind defs = block ind (Just $ DocEmptyLine) id (pElement defs)

pElement :: Definitions -> Parser DocElement
pElement defs
     =  pPrefLineEnvironment defs
    <|> pEnvironment         defs
    <|> pParagraph           defs

pPrefLineEnvironment :: Definitions -> Parser DocElement
pPrefLineEnvironment defs@Definitions{prefs, envs} = do
    ind'  <- indentLevel
    name  <- try $ pPrefix <* string " "
    pref  <- lookup name prefs `failMsg` "Unexpected prefix: " ++ unpack name
    let pPref = indentGuard sc EQ ind' *> string (name <> " ")
        pEl = do
            ind'' <- indentLevel
            pElements (unPos ind'' - 1) defs
    els   <- pEl `sepBy` try pPref
    return $ DocPrefGroup pref els

pParagraph :: Definitions -> Parser DocElement
pParagraph defs = do
    ind <- indentLevel
    let sep = newline >> indentGuard sc EQ ind >> notFollowedBy (void newline <|> eof <|> void (string "@"))
    DocParagraph <$> (pParLine `sepBy1` try sep) <* newline

pParLine :: Parser [ParEl]
pParLine = notFollowedBy (string "@") *> some (pText <|> pForm)
    where
        pText = ParText <$> takeWhile1P Nothing smbl
              <?> "Paragraph text"
        pForm = ParFormula
              <$> (char '`' *> takeWhile1P Nothing smbl <* char '`')
              <?> "Inline formula"
        smbl = (`notElem` ['`', '\n'])

pArgV :: ArgType -> Parser ArgV
pArgV ArgString = ArgVString <$> pStringLiteralL

pEnvironment :: Definitions -> Parser DocElement
pEnvironment defs@Definitions{envs} = do
    ind <- indentLevel
    try $ string "@"
    name <- pIdentifierL
    env <- lookup name envs `failMsg` ("Undefined environment " ++ show name)
    args <- mapM pArgV (atype <$> args env)
    sc <* newline
    DocEnvironment env args <$> pElements (unPos ind) defs

-- File readers and parsers

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

-- Printing .tex output
-- TODO : use pretty-print

texDoc :: Definitions -> [DocElement] -> Text
texDoc defs = (<> "\n") . texDocImpl defs False

texDocImpl :: Definitions -> Bool -> [DocElement] -> Text
texDocImpl defs math = T.intercalate "\n" . map (texDocElement defs math)


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: Maybe Text -> Maybe Text -> Bool -> Text -> Text
surround begin end empty txt = b <> txt <> e
    where
        b = fromMaybe T.empty $ if empty then begin else (<> "\n") <$> begin
        e = maybe T.empty ("\n" <> ) end

texDocElement :: Definitions -> Bool -> DocElement -> Text
texDocElement defs math (DocParagraph els)
        = T.intercalate "\n" $ map (mconcat . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = surround (repl <$> begin) (repl <$> end) (null els)
            $ texDocImpl defs (math || innerMath) els
    where
        repl = replaceArgs args argvs
texDocElement defs math (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround begin end (null els) bodyS
    where
        unM = fromMaybe T.empty
        sep'  = fromMaybe T.empty sep <> "\n"
        pref' = maybe T.empty (<> " ") pref
        bodyS = T.intercalate sep' ((pref' <>) . texDocImpl defs (math || innerMath) <$> els)
texDocElement _ _ DocEmptyLine = ""

texParEl :: Definitions -> Bool -> ParEl -> Text
texParEl _    False (ParText    t) = t
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Text
texMath Definitions{mathCmds} = foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds
