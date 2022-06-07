module Parser
  ( Pos,
    Parser,
    readDoc,
    Definitions (..),
    DocElement (..),
    Environment (..),
    ParEl (..),
    ParWord,
    Pref (..),
    ArgV (..),
    Argument (..),
    VerbMode (..),
    Command (..),
  )
where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad (replicateM_, unless, void)
import Control.Monad.Catch (MonadCatch, catchIOError)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), liftEither)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (readFile)
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.List.NonEmpty (fromList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import OptParser (OptParser, flagOpt, labelOpt, mkOptP, toParsec, (<??>), (<||>))
import System.FilePath
import Text.Megaparsec
  ( ErrorItem (Label),
    MonadParsec (eof, getParserState, label, lookAhead, notFollowedBy, takeWhile1P, takeWhileP, try),
    Parsec,
    SourcePos,
    anySingle,
    choice,
    errorBundlePretty,
    getOffset,
    getSourcePos,
    manyTill,
    manyTill_,
    mkPos,
    option,
    optional,
    parse,
    satisfy,
    sepBy,
    sepBy1,
    setErrorOffset,
    setParserState,
    someTill,
    unPos,
    unexpected,
    (<?>),
  )
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (char, eol, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (failMsg, listSepBy_, sepBy_, withError, (.:))
import Prelude hiding (fail, readFile)

-- Primitives

type Parser = Parsec Void Text

type Pos = (SourcePos, SourcePos)

-- | parse space or tab symbol
lineSpace :: Parser ()
lineSpace = void $ char ' ' <|> char '\t'

-- | parse line comment starting with '%' \\
--   comment can continue at the next line, if it starts with '%' too
lineComment :: Parser ()
lineComment = void $ p `sepBy1` sep
  where
    p = char '%' *> manyTill anySingle (lookAhead (void eol) <|> eof)
    sep = try (eol *> many lineSpace *> lookAhead (char '%'))

-- | line spaces or comments \\
--   'eol' after comment will be not consumed
sc :: Parser ()
sc = L.space lineSpace lineComment empty

-- | line spaces, comments or 'eol'
scn :: Parser ()
scn = L.space space1 lineComment empty

-- | Lexeme with line spaces or comments after it \\
--   'eol' after comment will be not consumed
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | String with spaces or comments after it
strLexeme :: Text -> Parser Text
strLexeme = lexeme . string

-- | String lexeme with `@` before it
atLexeme :: Text -> Parser Text
atLexeme = strLexeme . ("@" <>)

-- | Indentation of current pos, minimal 0
indentLevel :: Parser Int
indentLevel = (\p -> unPos p - 1) <$> L.indentLevel

-- | Parse string literal between `borderCh`
pStringBetween :: Char -> Parser Text
pStringBetween borderCh = do
  char borderCh
  (str, isEol) <- manyTill_ L.charLiteral (False <$ char borderCh <|> True <$ lookAhead eol)
  if isEol
    then unexpected . Label . fromList $ "end of line"
    else return $ T.pack str

-- | String literal lexeme between " and ' symbols
pStringLiteralL :: Parser Text
pStringLiteralL =
  lexeme (choice (pStringBetween <$> ['"', '\'']))
    <?> "String literal"

pIdentifierL :: Parser Text
pIdentifierL =
  lexeme (takeWhile1P Nothing (\ch -> isLetter ch || ch `elem` ['-', '\'']))
    <?> "Identifier"

pOperator :: Parser Text
pOperator =
  takeWhile1P Nothing (\ch -> not (isSpace ch) && ch `notElem` ['%', '`'])
    <?> "Operator"

pPrefix :: Parser Text
pPrefix =
  T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing chPred
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
pCommandL =
  pOperatorL <|> pIdentifierL
    <?> "Command"

data IndentOrd = IndGT | IndGEQ | IndEQ

indentGuard :: IndentOrd -> Int -> Parser ()
indentGuard ord ind =
  sc >> indentLevel >>= \ind' ->
    unless (ind' `comp` ind) . fail $
      "Incorrect indentation: " <> show ind'
        <> " should be "
        <> msg
        <> " "
        <> show ind
  where
    comp = case ord of
      IndGT -> (>)
      IndGEQ -> (>=)
      IndEQ -> (==)
    msg = case ord of
      IndGT -> "greater then"
      IndGEQ -> "at least"
      IndEQ -> "equal to"

recoverBind :: Parser a -> Parser b -> Parser b
recoverBind pa pb = do
  (mb, s) <- lookAhead $ do
    try pa
    mb <- optional pb
    s <- getParserState
    return (mb, s)
  case mb of
    Nothing -> empty
    Just b -> setParserState s >> return b

-- | Block of lines, parsed by `pel` with the same indentation
block :: Parser a -> Parser [a]
block pel = do
  ind <- indentLevel
  some $
    (indentGuard IndEQ ind >> notFollowedBy (void eol <|> eof))
      `recoverBind` pel

-- | Parse region with indentation `ord`, in relation to `ind` \\
--   Empty lines will be replaced by `eVal` (or ignored if it is `Nothing`) \\
--   Each line will be parsed by `pel`
region :: Maybe a -> Parser a -> IndentOrd -> Int -> Parser [a]
region eVal pel ord ind = (sep *> pel `sepBy_` sep) <|> pure []
  where
    sep = checkIndent *> sc *> ((*> eVal) <$> optional eol) <* scn
    checkIndent =
      lookAhead . try $
        scn *> notFollowedBy eof *> indentGuard ord ind

inArgsEnvironment :: Text -> Maybe el -> Parser args -> (args -> [el] -> a) -> Parser el -> Parser a
inArgsEnvironment name emptyEl pargs f pel =
  label ("Environment " ++ show name) $ do
    ind <- indentLevel
    atLexeme name
    args <- sc *> pargs <* sc <* eol <* sc
    f args <$> region emptyEl pel IndGT ind

inEnvironment :: Text -> Maybe el -> ([el] -> a) -> Parser el -> Parser a
inEnvironment name emptyEl f =
  inArgsEnvironment name emptyEl (return ()) (const f)

parseMapEl :: (Ord k, Show k) => Map k a -> String -> Parser k -> Parser a
parseMapEl m kType pk = do
  reg <- Megaparsec.region . setErrorOffset <$> getOffset
  k <- pk
  reg $ M.lookup k m `failMsg` "Undefined " <> kType <> " " <> show k

-- Definition block

data ArgType = ArgString
  deriving (Show)

type ArgName = Text

data Argument = Argument
  { atype :: ArgType,
    name :: ArgName
  }
  deriving (Show)

prettyArg :: Argument -> String
prettyArg Argument {atype, name} = "(" <> nameS <> " : " <> typeS <> ")"
  where
    nameS = T.unpack name
    typeS = case atype of
      ArgString -> "String"

newtype ArgV = ArgVString Text
  deriving (Show)

data VerbMode = NoVerb | Verb | VerbIndent
  deriving (Show)

data Environment = Environment
  { name :: Text,
    begin, end :: Maybe Text,
    args :: [Argument],
    innerMath :: Bool,
    innerVerb :: VerbMode,
    insidePref :: Bool
  }
  deriving (Show)

data Pref = Pref
  { name :: Text,
    begin, end :: Maybe Text,
    args :: [Argument],
    pref, sep :: Maybe Text,
    innerMath :: Bool,
    insidePref :: Bool,
    grouping :: Bool,
    oneLine :: Bool
  }
  deriving (Show)

data Command = Command
  { name, val :: Text
  }
  deriving (Show)

data Definition
  = DefE Environment
  | DefMC Command
  | DefC Command
  | DefP Pref
  deriving (Show)

pDefinitionBlock :: Parser [Definition]
pDefinitionBlock = skipImps *> scn *> (fromMaybe [] <$> optional pDefs)
  where
    pDefs = inEnvironment "Define" Nothing concat pDef
    pDef =
      map DefE <$> pEnvsDef
        <|> map DefMC <$> pMathCmdsDef
        <|> map DefP <$> pPrefDef
    skipImps = many $ inArgsEnvironment "Import" Nothing pStringLiteralL (\_ _ -> ()) (return ()) <* scn

pMathCmdsDef :: Parser [Command]
pMathCmdsDef = inEnvironment "MathCommands" Nothing id $ do
  name <- pCommandL
  strLexeme "="
  val <- pStringLiteralL
  eol
  return Command {name, val}

pType :: Parser ArgType
pType =
  ArgString <$ strLexeme "String"
    <?> "argument type `String`"

pDefArgs :: Parser [Argument]
pDefArgs = many . label "argument `(<name> : <type>)`" $ do
  try $ strLexeme "("
  name <- pIdentifierL
  strLexeme ":"
  atype <- pType
  strLexeme ")"
  return Argument {name, atype}

pBeginEndOpt :: OptParser (Maybe Text, Maybe Text)
pBeginEndOpt = option (Nothing, Nothing) $ texBEP <||> simpleBEP <??> ["@TexBeginEnd", "@Begin @End"]
  where
    texBEP = do
      texName <- mkOptP "TexBeginEnd" pStringLiteralL
      return (Just $ "\\begin{" <> texName <> "}", Just $ "\\end{" <> texName <> "}")
    simpleBEP = do
      begin <- optional $ mkOptP "Begin" pStringLiteralL
      end <- optional $ mkOptP "End" pStringLiteralL
      case (begin, end) of
        (Nothing, Nothing) -> empty
        _ -> return (begin, end)

pOpt :: OptParser a -> Parser a
pOpt = toParsec optNameP optArgsConsumer
  where
    optNameP = try (string "@" >> pIdentifierL) <?> "option name `@<name>`"
    optArgsConsumer = takeWhileP Nothing (`notElem` ['@', '\n', '\r', '%'])

pEnvsDef :: Parser [Environment]
pEnvsDef = inEnvironment "Environments" Nothing id $ do
  name <- pIdentifierL
  args <- pDefArgs
  strLexeme "="
  ((begin, end), innerMath, innerVerb, insidePref) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      math <- flagOpt "Math"
      verb <-
        option
          NoVerb
          ( (Verb <$ labelOpt "Verb")
              <||> (VerbIndent <$ labelOpt "VerbIndent")
              <??> ["Verb", "VerbIndent"]
          )
      insidePref <- not <$> flagOpt "NoPrefInside"
      return (beginEnd, math, verb, insidePref)
  eol
  return Environment {name, begin, end, args, innerMath, innerVerb, insidePref}

pPrefDef :: Parser [Pref]
pPrefDef = inEnvironment "Prefs" Nothing id $ do
  name <- pPrefixL
  args <- pDefArgs
  strLexeme "="
  ((begin, end), pref, sep, innerMath, insidePref, grouping, oneLine) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      pref <- optional (mkOptP "Pref" pStringLiteralL)
      sep <- optional (mkOptP "Sep" pStringLiteralL)
      math <- flagOpt "Math"
      insidePref <- not <$> flagOpt "NoPrefInside"
      grouping <- not <$> flagOpt "NoGroup"
      oneLine <- flagOpt "OneLine"
      -- let grouping = False
      --     oneLine = False
      return (beginEnd, pref, sep, math, insidePref, grouping, oneLine)
  eol
  return Pref {name, begin, end, args, pref, sep, innerMath, insidePref, grouping, oneLine}

-- Processing definitions

data Definitions = Definitions
  { envs :: Map Text Environment,
    cmds :: Map Text Command,
    mathCmds :: Map Text Command,
    prefs :: Map Text Pref
  }
  deriving (Show)

instance Semigroup Definitions where
  (Definitions a b c d) <> (Definitions a' b' c' d') =
    Definitions (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid Definitions where
  mempty = Definitions mempty mempty mempty mempty

processDefs :: [Definition] -> Definitions
processDefs = mconcat . map processDef

processDef :: Definition -> Definitions
processDef (DefE env@Environment {name}) =
  mempty {envs = M.singleton name env}
processDef (DefC cmd@Command {name}) =
  mempty {cmds = M.singleton name cmd}
processDef (DefMC cmd@Command {name}) =
  mempty {mathCmds = M.singleton name cmd}
processDef (DefP pr@Pref {name}) =
  mempty {prefs = M.singleton name pr}

-- Document body

data DocElement
  = DocParagraph [[ParEl]]
  | DocEnvironment Environment [ArgV] [DocElement]
  | DocPrefGroup Pref [([ArgV], [DocElement])]
  | DocEmptyLine
  | DocVerb Bool [Text]
  deriving (Show)

type ParWord = (Text, Pos)

data ParEl
  = ParText [ParWord] -- List of words
  | ParFormula [ParWord]
  deriving (Show)

pDocument :: Definitions -> Parser [DocElement]
pDocument defs = scn *> pElements True IndGEQ 0 defs <* scn

pElements :: Bool -> IndentOrd -> Int -> Definitions -> Parser [DocElement]
pElements enPref ord ind defs = region (Just DocEmptyLine) (pElement enPref defs) ord ind

pElement :: Bool -> Definitions -> Parser DocElement
pElement enPref defs =
  choice $
    [pPrefLineEnvironment defs | enPref]
      <> [ pEnvironment defs,
           pParagraph defs
         ]

pPrefLineEnvironment :: Definitions -> Parser DocElement
pPrefLineEnvironment defs@Definitions {prefs} = do
  ind <- indentLevel
  pref@Pref {insidePref, name, grouping, args} <-
    lookAhead $
      parseMapEl prefs "prefix" (try $ pPrefix <* sp)
  let pel = do
        try (string name <* sp)
        args <- mapM pArgV args
        body <- pElements insidePref IndGT ind defs
        return (args, body)
  if grouping
    then DocPrefGroup pref <$> block pel
    else DocPrefGroup pref . (: []) <$> pel
  where
    sp = string " " <|> eol

pParagraph :: Definitions -> Parser DocElement
pParagraph _ = DocParagraph <$> block pParLine

pParLine :: Parser [ParEl]
pParLine = notFollowedBy (string "@") *> someTill (pForm <|> pText <|> pEmptyText) (try $ sc <* eol)
  where
    word :: Parser ParWord
    word = do
      begin <- getSourcePos
      val <- takeWhile1P Nothing smbl
      end <- getSourcePos
      return (val, (begin, end))
    sp :: Parser Char
    sp = char ' '
    pEmptyText = do
      pos <- getSourcePos
      some sp
      return $ ParText $ [(" ", (pos, pos))]
    pText = label "Paragraph text" $ do
      try $ lookAhead (many sp *> satisfy smbl)
      bPos <- getSourcePos
      bSpace <- optional sp
      words <- some $ do
        try $ lookAhead (many sp *> satisfy smbl)
        many sp *> word
      ePos <- getSourcePos
      eSpace <- optional . try $ do
        lookAhead (sc *> notFollowedBy eol)
        sp
      many sp
      return $ ParText $ h bPos bSpace <> words <> h ePos eSpace
    h p (Just _) = [(T.empty, (p, p))]
    h _ Nothing = []
    pForm = label "Inline formula" $ do
      char '`'
      many sp
      words <- many (word <* many sp)
      char '`'
      return $ ParFormula words
    smbl = (`notElem` ['`', '\r', '\n', '%', ' '])

pArgV :: Argument -> Parser ArgV
pArgV arg = label (prettyArg arg) $ case atype arg of
  ArgString -> ArgVString <$> pStringLiteralL

pVerb :: Bool -> Int -> Parser DocElement
pVerb verbInd ind =
  DocVerb verbInd <$> do
    indGuard
    ind' <- indentLevel
    pLine `listSepBy_` sep ind'
  where
    pLine = (: []) . T.pack <$> manyTill anySingle eol
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
pEnvironment defs@Definitions {envs} = do
  ind <- indentLevel
  env@Environment {innerVerb, insidePref, args} <-
    parseMapEl envs "environment" (string "@" *> pIdentifierL)
  args <- mapM pArgV args
  sc <* eol
  DocEnvironment env args
    <$> case innerVerb of
      NoVerb -> pElements insidePref IndGT ind defs
      Verb -> (: []) <$> pVerb False ind
      VerbIndent -> (: []) <$> pVerb True ind

-- File readers and parsers

-- | Parse file with given definitions from imported files
pFile :: Definitions -> Parser (Definitions, [DocElement])
pFile impDefs = do
  defs <- processDefs <$> pDefinitionBlock
  let defs' = impDefs <> defs
  doc <- pDocument defs'
  return (defs', doc)

-- | Parse import filenames
pImportFilenames :: Parser [FilePath]
pImportFilenames =
  L.nonIndented scn $
    try (atLexeme "Import" *> fmap unpack pStringLiteralL `sepBy` try (eol *> scn *> atLexeme "Import"))
      <|> return []

scanImportFilenames :: FilePath -> Text -> Either String [FilePath]
scanImportFilenames =
  first (("Error while reading imports: " <>) . errorBundlePretty)
    .: parse pImportFilenames

readDoc ::
  (MonadError String m, MonadIO m, MonadCatch m) =>
  FilePath ->
  m (Text, (Definitions, [DocElement]))
readDoc fileName = do
  file <-
    decodeUtf8 <$> liftIO (readFile fileName)
      `catchIOError` \e ->
        throwError ("Unable to open file '" <> fileName <> "': " <> show e)
  impFNames <- liftEither $ scanImportFilenames fileName file
  defs <-
    mconcat
      <$> mapM
        ((fst . snd <$>) . withError addPrefix . readDoc . (dropFileName fileName <>))
        impFNames
  case parse (pFile defs) fileName file of
    Left e -> throwError $ errorBundlePretty e
    Right res -> return (file, res)
  where
    addPrefix eStr = "While processing imports from " <> fileName <> ":\n" <> eStr
