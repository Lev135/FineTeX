module Generator where

import Prelude hiding (readFile, fail)
import Utils ( sepBy_, failMsg, (.:), withError, eitherFail, listSepBy_ )
import OptParser ( OptParser, (<||>), (<??>), mkOptP, toParsec, flagOpt, labelOpt )
import Text.Megaparsec(Parsec, MonadParsec (takeWhileP, label, takeWhile1P, try, notFollowedBy, lookAhead, eof, getParserState), Pos, sepBy1, sepBy, unPos, (<?>), choice, optional, parse, errorBundlePretty, mkPos, satisfy, manyTill, option, anySingle, someTill, setParserState, unexpected, ErrorItem (Label), manyTill_)
import Text.Megaparsec.Char ( char, space1, eol, letterChar, string )
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void(Void)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Monad ( void, when, unless, join, guard, forM, replicateM, (>=>), replicateM_ )
import Control.Applicative ( Alternative(empty, (<|>), some, many) )
import Data.Maybe (maybeToList, isJust, mapMaybe, fromMaybe)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Char (isLetter, isSpace, isAlphaNum)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), liftEither)
import Control.Monad.Catch (MonadCatch, catchIOError)
import Data.ByteString (readFile)
import Control.Monad.Fail (MonadFail (fail))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import Data.Map (Map)
import Text.Megaparsec.Debug (dbg)
import Data.List.NonEmpty (fromList)

-- Primitives

type Parser = Parsec Void Text

lineSpace :: Parser ()
lineSpace = void $ char ' ' <|> char '\t'

lineComment :: Parser ()
lineComment = do
    p
    void . many . try $ eol *> many lineSpace *> p
    where
        p = char '%' *> manyTill anySingle (lookAhead (void eol) <|> eof)

sc :: Parser ()
sc = L.space lineSpace lineComment empty

scn :: Parser ()
scn = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

indentLevel :: Parser Int
indentLevel = (\p -> unPos p - 1) <$> L.indentLevel

strLexeme :: Text -> Parser Text
strLexeme = lexeme . string

atLexeme :: Text -> Parser Text
atLexeme = strLexeme . ("@" <>)

pStringBetween :: Char -> Parser Text
pStringBetween bCh = do
        bChP
        (str, isEol) <- manyTill_ L.charLiteral (False <$ bChP <|> True <$ lookAhead eol)
        if isEol
        then unexpected (Label $ fromList "end of line")
        else return $ T.pack str
    where
        bChP = char bCh


pStringLiteralL :: Parser Text
pStringLiteralL = lexeme (choice (pStringBetween <$> ['"', '\'']))
    <?> "String literal"

pIdentifierL :: Parser Text
pIdentifierL = lexeme (takeWhile1P Nothing (\ch -> isLetter ch || ch `elem` ['-', '\'']))
    <?> "Identifier"

pOperator :: Parser Text
pOperator = takeWhile1P Nothing (\ch -> not (isSpace ch) && ch `notElem` ['%', '`'])
    <?> "Operator"

pPrefix :: Parser Text
pPrefix = T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing chPred
    <?> "Prefix"
    where
        fstSmbls :: [Char]
        fstSmbls = "!#$%^*-+,./|\\><[]~"
        chPred ch = not $ isSpace ch || isAlphaNum ch || ch == '%'

pPrefixL :: Parser Text
pPrefixL = lexeme pPrefix

pOperatorL :: Parser Text
pOperatorL = lexeme pOperator

pCommandL :: Parser Text
pCommandL = pOperatorL <|> pIdentifierL
    <?> "Command"

data IndentOrd = IndGT | IndGEQ | IndEQ

indentGuard :: IndentOrd -> Int -> Parser ()
indentGuard ord ind = sc >> indentLevel >>= \ind' ->
    unless (ind' `comp` ind) . fail
        $  "Incorrect indentation: " <> show ind'
        <> " should be " <> msg <> " " <> show ind
    where
        comp = case ord of
            IndGT  -> (>)
            IndGEQ -> (>=)
            IndEQ  -> (==)
        msg = case ord of
            IndGT  -> "greater then"
            IndGEQ -> "at least"
            IndEQ  -> "equal to"

recoverBind :: Parser a -> Parser b -> Parser b
recoverBind pa pb = do
    (mb, s) <- lookAhead $ do
        try pa
        mb <- optional pb
        s <- getParserState
        return (mb, s)
    case mb of
        Nothing -> empty
        Just b  -> setParserState s >> return b

block :: Parser a -> Parser [a]
block pel = do
    ind <- indentLevel
    some $ (indentGuard IndEQ ind >> notFollowedBy (void eol <|> eof)) `recoverBind` pel

block' :: Maybe a -> Parser a -> IndentOrd -> Int -> Parser [a]
block' eVal pel ord ind = (sep *> pel `sepBy_` sep) <|> pure []
    where
        sep = checkIndent *> sc *> (( *> eVal) <$> optional eol) <* scn
        checkIndent = lookAhead . try
                $ scn *> notFollowedBy eof *> indentGuard ord ind

inArgsEnvironment :: Text -> Maybe el -> Parser args -> (args -> [el] -> a) -> Parser el -> Parser a
inArgsEnvironment name emptyEl pargs f pel
    = label ("Environment " ++ show name) $ do
        ind <- indentLevel
        atLexeme name
        args <- sc *> pargs <* sc <* eol <* sc
        f args <$> block' emptyEl pel IndGT ind

inEnvironment :: Text -> Maybe el -> ([el] -> a) -> Parser el -> Parser a
inEnvironment name emptyEl f
    = inArgsEnvironment name emptyEl (return ()) (const f)


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

data VerbMode = NoVerb | Verb | VerbIndent
    deriving Show

data Environment = Environment {
        name                :: Text,
        begin, end          :: Maybe Text,
        args                :: [Argument],
        innerMath           :: Bool,
        innerVerb           :: VerbMode,
        insidePref          :: Bool
    }
    deriving Show
data Pref = Pref {
        name        :: Text,
        begin, end  :: Maybe Text,
        pref,  sep  :: Maybe Text,
        innerMath   :: Bool,
        insidePref  :: Bool
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
    eol
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


-- option (Nothing, Nothing) $    -- лишнее, вроде бы
pBeginEndOpt :: OptParser (Maybe Text, Maybe Text)
pBeginEndOpt = option (Nothing, Nothing) $ texBEP <||> simpleBEP <??> ["@TexBeginEnd", "@Begin @End"]
    where
        texBEP = do
            texName <- mkOptP "TexBeginEnd" pStringLiteralL
            return (Just $ "\\begin{" <> texName <> "}", Just $ "\\end{"   <> texName <> "}")
        simpleBEP = do
            begin <- optional $ mkOptP "Begin" pStringLiteralL
            end   <- optional $ mkOptP "End" pStringLiteralL
            case (begin, end) of
                (Nothing, Nothing) -> empty
                _ -> return (begin, end)

pOpt :: OptParser a -> Parser a
pOpt = toParsec optNameP optArgsConsumer
    where
        optNameP = try (string "@") >> pIdentifierL
        optArgsConsumer = takeWhileP Nothing (`notElem` ['@', '\n', '\r', '%'])

pEnvsDef :: Parser [Environment]
pEnvsDef = inEnvironment "Environments" Nothing id $ do
        name         <- pIdentifierL
        args         <- pDefArgs
        strLexeme    "="
        ((begin, end), innerMath, innerVerb, insidePref)
            <- pOpt $ do
                beginEnd     <- pBeginEndOpt
                math         <- flagOpt "Math"
                verb         <- option NoVerb (
                                (Verb <$ labelOpt "Verb")
                           <||> (VerbIndent <$ labelOpt "VerbIndent")
                           <??> ["Verb", "VerbIndent"]
                        )
                insidePref   <- not <$> flagOpt "NoPrefInside"
                return (beginEnd, math, verb, insidePref)
        eol
        return Environment{ name, begin, end, args, innerMath, innerVerb, insidePref }

pPrefDef :: Parser [Pref]
pPrefDef = inEnvironment "Prefs" Nothing id $ do
        name      <- pPrefixL
        strLexeme "="
        ((begin, end), pref, sep, innerMath, insidePref)
            <- pOpt $ do
                beginEnd    <- pBeginEndOpt
                pref        <- optional (mkOptP "Pref" pStringLiteralL)
                sep         <- optional (mkOptP "Sep"  pStringLiteralL)
                math        <- flagOpt "Math"
                insidePref  <- not <$> flagOpt "NoPrefInside"
                return (beginEnd, pref, sep, math, insidePref)
        eol
        return Pref{name, begin, end, pref, sep, innerMath, insidePref}

-- Processing definitions

data Definitions = Definitions {
        envs         :: Map Text Environment,
        cmds         :: Map Text Command,
        mathCmds     :: Map Text Command,
        prefs        :: Map Text Pref
    }
    deriving Show

instance Semigroup Definitions where
    (Definitions a b c d) <> (Definitions a' b' c' d')
        = Definitions (a <> a') (b <> b') (c <> c') (d <> d')
instance Monoid Definitions where
    mempty = Definitions mempty mempty mempty mempty

processDefs :: [Definition] -> Definitions
processDefs = mconcat . map processDef

processDef :: Definition -> Definitions
processDef (DefE env@Environment{name, begin, end, innerMath})
    = mempty { envs     = M.singleton name env }
processDef (DefC cmd@Command{name})
    = mempty { cmds     = M.singleton name cmd }
processDef (DefMC cmd@Command{name})
    = mempty { mathCmds = M.singleton name cmd }
processDef (DefP pr@Pref{name})
    = mempty {prefs     = M.singleton name pr }

-- Document body

data DocElement
    = DocParagraph     [[ParEl]]
    | DocEnvironment   Environment [ArgV] [DocElement]
    | DocPrefGroup     Pref [[DocElement]]
    | DocEmptyLine
    | DocVerb          Bool [Text]
    deriving Show

data ParEl
    = ParText       [Text] -- List of words
    | ParFormula    [Text]
    deriving Show

pDocument :: Definitions -> Parser [DocElement]
pDocument defs = scn *> pElements True IndGEQ 0 defs <* scn

pElements :: Bool -> IndentOrd -> Int -> Definitions -> Parser [DocElement]
pElements enPref ord ind defs = block' (Just DocEmptyLine) (pElement enPref defs) ord ind

pElement :: Bool -> Definitions -> Parser DocElement
pElement enPref defs = choice $
       [pPrefLineEnvironment defs | enPref ]
    <> [pEnvironment         defs
    ,   pParagraph           defs ]

pPrefLineEnvironment :: Definitions -> Parser DocElement
pPrefLineEnvironment defs@Definitions{prefs} = do
    s <- getParserState
    (name, ind)  <- (,) <$> try (pPrefix <* string " ") <*> indentLevel
    pref@Pref{insidePref}  <- M.lookup name prefs `failMsg` "Unexpected prefix: " ++ unpack name
    setParserState s
    DocPrefGroup pref <$> block (try (string $ name <> " ") *> pElements insidePref IndGEQ ind defs)

pParagraph :: Definitions -> Parser DocElement
pParagraph defs = do
    ind <- indentLevel
    DocParagraph <$> block pParLine

words' :: Text -> [Text]
words' t = h (T.head t) <> T.words t <> h (T.last t)
    where
        h ' ' = [T.empty]
        h _   = []

pParLine :: Parser [ParEl]
pParLine = notFollowedBy (string "@") *> some (pText <|> pForm) <* sc <* eol
    where
        pText = ParText <$> (words' <$> takeWhile1P Nothing smbl)
              <?> "Paragraph text"
        pForm = ParFormula
              <$> (char '`' *> (T.words <$> takeWhile1P Nothing smbl) <* char '`')
              <?> "Inline formula"
        smbl = (`notElem` ['`', '\n', '%'])

pArgV :: ArgType -> Parser ArgV
pArgV ArgString = ArgVString <$> pStringLiteralL

pVerb :: Bool -> Int -> Parser DocElement
pVerb verbInd ind = DocVerb verbInd <$> do
        indGuard
        ind' <- indentLevel
        pLine `listSepBy_` sep ind'
    where
        pLine = (:[]) . T.pack <$> manyTill anySingle eol
        sep :: Int -> Parser [Text]
        sep ind' = do
            lookAhead . try $ many space1 >> indGuard
            emptyLs <- many (try $ T.empty <$ many lineSpace <* eol)
            lookAhead indGuard
            replicateM_ ind' $ char ' '
            return emptyLs
        indGuard = L.indentGuard (void $ many lineSpace) GT posInd
        posInd = mkPos $ ind + 1

pEnvironment :: Definitions -> Parser DocElement
pEnvironment defs@Definitions{envs} = do
    ind <- indentLevel
    name <- string "@" *> pIdentifierL
    env@Environment{innerVerb, insidePref}
        <- M.lookup name envs `failMsg` ("Undefined environment " ++ show name)
    args <- mapM pArgV (atype <$> args env)
    sc <* eol
    DocEnvironment env args <$>
        case innerVerb of
            NoVerb      -> pElements insidePref IndGT ind defs
            Verb        -> (:[]) <$> pVerb False ind
            VerbIndent  -> (:[]) <$> pVerb True ind

-- File readers and parsers

pFile :: Definitions -> Parser (Definitions, [DocElement])
pFile impDefs = do
    defs <- processDefs <$> pDefinitionBlock
    let defs' = impDefs <> defs
    doc  <- pDocument defs'
    return (defs', doc)

pImportFiles :: Parser [FilePath]
pImportFiles = L.nonIndented scn $
    try (atLexeme "Import" *> fmap unpack pStringLiteralL `sepBy` try (eol *> scn *> atLexeme "Import"))
        <|> return []

getImports :: FilePath -> Text -> Either String [FilePath]
getImports = first (("get imports error: " <>) . errorBundlePretty) .: parse pImportFiles

readDoc :: (MonadError String m, MonadIO m, MonadCatch m) => FilePath -> m (Definitions, [DocElement])
readDoc fileName = do
    file      <- decodeUtf8 <$> liftIO (readFile fileName)
        `catchIOError` \e -> throwError ("Unable to open file '" <> fileName <> "': " <> show e)
    impFNames <- liftEither $ getImports fileName file
    defs      <- mconcat <$> mapM ((fst <$>) . withError addPrefix . readDoc) impFNames
    case parse (pFile defs) fileName file of
        Left   e   -> throwError $ errorBundlePretty e
        Right  res -> return res
    where
        addPrefix eStr = "While processing imports from " <> fileName <> ":\n" <> eStr
