{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser
  ( Parser,
    Posed,
    readDoc,
    Definitions (..),
    DocElement (..),
    Environment (..),
    ParEl (..),
    Inline (..),
    ParWord,
    Pref (..),
    ArgType (..),
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
import Control.Monad.RWS (MonadState (..), MonadWriter (..), gets, modify)
import Control.Monad.State (runState)
import Control.Monad.Writer (WriterT (..))
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (readFile)
import Data.Char (isLetter, isSpace)
import Data.Function (on)
import Data.List.NonEmpty (fromList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import OptParser (OptParser, flagOpt, labelOpt, mkOptP, toParsec, (<??>), (<||>))
import qualified Prettyprinter as P
import System.FilePath (dropFileName)
import Text.Megaparsec
  ( ErrorItem (Label),
    MonadParsec (eof, getParserState, label, lookAhead, notFollowedBy, takeWhile1P, takeWhileP, try),
    ParseErrorBundle,
    Parsec,
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
    runParser',
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
import Utils (Box (..), Pos, PosC (..), PrettyErr (..), Sources, failMsg, foldMapBox, listSepBy_, prettyPos, sepBy_, sequenceBox, withError)
import Prelude hiding (fail, readFile)

-- Primitives

type Parser = Parsec Void Text

withPos :: Parser a -> Parser (a, Pos)
withPos pa = do
  b <- getSourcePos
  a <- pa
  e <- getSourcePos
  return (a, (b, e))

data Posed a = Posed {val :: a, pos :: Pos}

instance Eq a => Eq (Posed a) where
  (==) = (==) `on` unBox

instance Ord a => Ord (Posed a) where
  (<=) = (<=) `on` unBox

instance Show a => Show (Posed a) where
  show Posed {val, pos = (b, e)} = show (b, e) <> ": " <> show val

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

withPos' :: Parser a -> Parser (Posed a)
withPos' pa = do
  b <- getSourcePos
  a <- pa
  e <- getSourcePos
  return $ Posed a (b, e)

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
  takeWhile1P Nothing (\ch -> not (isSpace ch) && ch /= '%')
    <?> "Operator"

pPrefix :: Parser Text
pPrefix =
  T.cons <$> satisfy (`elem` fstSmbls) <*> takeWhileP Nothing chPred
    <?> "Prefix"
  where
    fstSmbls :: [Char]
    fstSmbls = "!#$%^*-+,./|><[]~"
    chPred ch = not $ isSpace ch || ch == '%'

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
region eVal pel ord ind =
  ((sep *> pel `sepBy_` sep) <|> [] <$ (sc *> optional eol))
    <* notFollowedBy sep
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

data ArgType = ArgString | ArgMath
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
      ArgMath -> "Math"

data ArgV = ArgVString Text | ArgVMath [ParWord]
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
    pref, suf, sep :: Maybe Text,
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

type Borders = (Text, Text)

data Inline = Inline
  { borders :: Borders,
    begin, end :: Maybe Text,
    innerMath :: Bool
  }
  deriving (Show, Eq, Ord)

data Definition
  = DefE Environment
  | DefMC Command
  | DefP Pref
  | DefI Inline
  deriving (Show)

pDefinitionBlock :: Parser [Posed Definition]
pDefinitionBlock = fromMaybe [] <$> optional pDefs
  where
    pDefs = inEnvironment "Define" Nothing concat pDef
    pDef =
      map (fmap DefE) <$> pEnvsDef
        <|> map (fmap DefMC) <$> pMathCmdsDef
        <|> map (fmap DefP) <$> pPrefDef
        <|> map (fmap DefI) <$> pInlineDef

pMathCmdsDef :: Parser [Posed Command]
pMathCmdsDef = inEnvironment "MathCommands" Nothing id . withPos' $ do
  name <- pCommandL
  strLexeme "="
  val <- pStringLiteralL
  eol
  return Command {name, val}

pType :: Parser ArgType
pType =
  ArgString <$ strLexeme "String"
    <|> ArgMath <$ strLexeme "Math"
      <?> "argument type `String` | `Math`"

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

pEnvsDef :: Parser [Posed Environment]
pEnvsDef = inEnvironment "Environments" Nothing id . withPos' $ do
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

pPrefDef :: Parser [Posed Pref]
pPrefDef = inEnvironment "Prefs" Nothing id . withPos' $ do
  name <- pPrefixL
  args <- pDefArgs
  strLexeme "="
  ((begin, end), pref, suf, sep, innerMath, insidePref, grouping, oneLine) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      pref <- optional (mkOptP "Pref" pStringLiteralL)
      suf <- optional (mkOptP "Suf" pStringLiteralL)
      sep <- optional (mkOptP "Sep" pStringLiteralL)
      math <- flagOpt "Math"
      insidePref <- not <$> flagOpt "NoPrefInside"
      grouping <- not <$> flagOpt "NoGroup"
      oneLine <- flagOpt "OneLine"
      return (beginEnd, pref, suf, sep, math, insidePref, grouping, oneLine)
  eol
  return Pref {name, begin, end, args, pref, suf, sep, innerMath, insidePref, grouping, oneLine}

pInlineDef :: Parser [Posed Inline]
pInlineDef = inEnvironment "Inlines" Nothing id . withPos' $ do
  startStr <- pCommandL
  endStr <- pCommandL
  strLexeme "="
  (begin, end, innerMath) <- pOpt $ do
    (begin, end) <- pBeginEndOpt
    innerMath <- flagOpt "Math"
    return (begin, end, innerMath)
  return Inline {borders = (startStr, endStr), begin, end, innerMath}

-- Processing definitions

data Definitions p = Definitions
  { envs :: Map Text (p Environment),
    mathCmds :: Map Text (p Command),
    prefs :: Map Text (p Pref),
    inlines :: Set (p Inline),
    fstInlineChs :: Set Char
  }

deriving instance (forall a. Show a => Show (p a), Box p) => Show (Definitions p)

instance Box p => Semigroup (Definitions p) where
  (Definitions a b c d e) <> (Definitions a' b' c' d' e') =
    Definitions (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Box p => Monoid (Definitions p) where
  mempty = Definitions mempty mempty mempty mempty mempty

data DefType = DefTEnv | DefTMathCmd | DefTPref

data ProcessDefsError
  = MultipleDecl DefType Text Pos Pos

instance PrettyErr ProcessDefsError where
  prettyErr src (MultipleDecl t name p p') =
    P.vsep
      [ prettyPos src lbl p,
        prettyPos src "Previously defined here" p'
      ]
    where
      lbl = "Multiple definitions of " <> tName <> " '" <> name <> "'"
      tName = case t of
        DefTEnv -> "environment"
        DefTMathCmd -> "command"
        DefTPref -> "pref"

type MState m p =
  (MonadWriter [ProcessDefsError] m, MonadState (Definitions p) m)

processDefs :: forall m p. (MState m p, PosC p) => [p Definition] -> m ()
processDefs = mapM_ h
  where
    h :: p Definition -> m ()
    h pdef = case unBox pdef of
      DefE env@Environment {name} -> do
        menv' <- gets $ M.lookup name . envs
        case menv' of
          Nothing ->
            modify $ \defs@Definitions {envs} ->
              defs {envs = M.insert name (env <$ pdef) envs}
          Just penv' ->
            tell [MultipleDecl DefTEnv name (getPos pdef) (getPos penv')]
      DefMC cmd@Command {name} -> do
        mcmd' <- gets $ M.lookup name . mathCmds
        case mcmd' of
          Nothing ->
            modify $ \defs@Definitions {mathCmds} ->
              defs {mathCmds = M.insert name (cmd <$ pdef) mathCmds}
          Just pcmd' ->
            tell [MultipleDecl DefTMathCmd name (getPos pdef) (getPos pcmd')]
      DefP pref@Pref {name} -> do
        mpref' <- gets $ M.lookup name . envs
        case mpref' of
          Nothing ->
            modify $ \defs@Definitions {prefs} ->
              defs {prefs = M.insert name (pref <$ pdef) prefs}
          Just ppref' ->
            tell [MultipleDecl DefTPref name (getPos pdef) (getPos ppref')]
      DefI inline@Inline {borders} -> do
        -- TODO add errors
        modify $ \defs@Definitions {inlines} ->
          defs {inlines = S.insert (inline <$ pdef) inlines}
        let fstCh = T.head $ fst borders
        modify $ \defs@Definitions {fstInlineChs} ->
          defs {fstInlineChs = S.insert fstCh fstInlineChs}

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
  | ParInline Inline [ParWord]
  deriving (Show)

pDocument :: Box p => Definitions p -> Parser [DocElement]
pDocument defs = scn *> pElements True IndGEQ 0 defs

pElements :: Box p => Bool -> IndentOrd -> Int -> Definitions p -> Parser [DocElement]
pElements enPref ord ind defs = region (Just DocEmptyLine) (pElement enPref defs) ord ind

pElement :: Box p => Bool -> Definitions p -> Parser DocElement
pElement enPref defs =
  choice $
    [pPrefLineEnvironment defs | enPref]
      <> [ pEnvironment defs,
           pParagraph defs
         ]

pPrefLineEnvironment :: Box p => Definitions p -> Parser DocElement
pPrefLineEnvironment defs@Definitions {prefs} = do
  pref@Pref {insidePref, name, grouping, args} <-
    lookAhead $
      unBox <$> parseMapEl prefs "prefix" pPrefix
  let pel = do
        ind <- indentLevel
        strLexeme name
        args <- mapM pArgV args
        body <- pElements insidePref IndGT ind defs
        return (args, body)
  els <- if grouping then block pel else (: []) <$> pel
  return $ DocPrefGroup pref els

pParagraph :: Box p => Definitions p -> Parser DocElement
pParagraph defs = DocParagraph <$> block (pParLine defs)

smbl :: Set Char -> Char -> Bool
smbl fstInlineChs = (`notElem` S.union fstInlineChs (S.fromList ['\r', '\n', '%', ' ']))

pWord :: Set Char -> Parser ParWord
pWord fstInlineChs = withPos $ takeWhile1P Nothing $ smbl fstInlineChs

pForm :: Parser [ParWord]
pForm =
  char '`' *> sc *> many (pWord S.empty <* sc) <* char '`'
    <?> "Inline formula"

pInline :: Box p => Definitions p -> Parser ParEl
pInline Definitions {inlines} = choice $ h . unBox <$> S.toList inlines
  where
    h :: Inline -> Parser ParEl
    h inl@Inline {borders = (beg, end)} =
      ParInline inl
        <$> ( string beg *> sc
                *> many (pWord (S.singleton $ T.head end) <* sc)
                <* string end
            )

pParLine :: Box p => Definitions p -> Parser [ParEl]
pParLine defs@Definitions {fstInlineChs} =
  notFollowedBy (string "@") *> someTill pEl (try $ sc <* eol)
  where
    pEl = do
      ch <- lookAhead anySingle
      if ch `S.member` fstInlineChs
        then pInline defs
        else
          ParText <$> pText
            <|> ParText <$> pEmptyText
    sps = (\pos -> (T.empty, (pos, pos))) <$> getSourcePos <* some (char ' ')
    pEmptyText = (: []) . first (const " ") <$> sps
    pText = label "Paragraph text" $ do
      try $ lookAhead (optional sps *> satisfy (smbl fstInlineChs))
      bSp <- fmap h . maybeToList <$> optional sps
      words <- pWord fstInlineChs `sepBy` try (sps *> lookAhead (satisfy $ smbl fstInlineChs))
      eSp <- try (fmap h . maybeToList <$> optional sps <* notFollowedBy eol) <|> pure []
      return $ bSp <> words <> eSp
    h = id

pArgV :: Argument -> Parser ArgV
pArgV arg = label (prettyArg arg) $ case atype arg of
  ArgString -> ArgVString <$> pStringLiteralL
  ArgMath -> ArgVMath <$> pForm

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

pEnvironment :: Box p => Definitions p -> Parser DocElement
pEnvironment defs@Definitions {envs} = do
  ind <- indentLevel
  env@Environment {innerVerb, insidePref, args} <-
    unBox <$> parseMapEl envs "environment" (string "@" *> pIdentifierL)
  args <- mapM pArgV args
  sc <* eol
  DocEnvironment env args
    <$> case innerVerb of
      NoVerb -> pElements insidePref IndGT ind defs
      Verb -> (: []) <$> pVerb False ind
      VerbIndent -> (: []) <$> pVerb True ind

-- File readers and parsers

-- | Parse import filenames
pImportFilenames :: Parser [Posed FilePath]
pImportFilenames =
  L.nonIndented scn $
    withPos' (inArgsEnvironment "Import" Nothing pStringLiteralL (\t _ -> unpack t) (return ()) <* scn)
      `sepBy` (try . lookAhead $ atLexeme "Import")

data ReadDocError
  = OpenFileError FilePath IOError
  | ParseError (ParseErrorBundle Text Void)
  | ImportError FilePath Pos ReadDocError
  | ProcessError [ProcessDefsError]

instance PrettyErr ReadDocError where
  prettyErr src e = case e of
    OpenFileError file ioE ->
      "Unable to open file '" <> P.pretty file <> "': " <> P.pretty (show ioE)
    ParseError peb -> "Parse error: " <> P.pretty (errorBundlePretty peb)
    ImportError file p rde ->
      P.vsep
        [ prettyPos src ("In '" <> T.pack file <> "' imported from here") p,
          prettyErr src rde
        ]
    ProcessError procErrs ->
      P.vsep (prettyErr src <$> procErrs)

readDoc ::
  (MonadError ReadDocError m, MonadIO m, MonadCatch m, MonadWriter Sources m) =>
  FilePath ->
  m (Definitions Posed, [DocElement])
readDoc fileName = do
  file <-
    decodeUtf8 <$> liftIO (readFile fileName)
      `catchIOError` \e ->
        throwError (OpenFileError fileName e)
  tell $ M.singleton fileName (T.lines file)
  (impFNames, state') <-
    liftEither $
      first ParseError $
        parse ((,) <$> pImportFilenames <*> getParserState) fileName file
  defs <-
    let readDoc' pf = withError (ImportError (unBox pf) (getPos pf)) (readDoc (unBox pf))
     in mconcat
          <$> mapM
            ((fst <$>) . readDoc' . fmap (dropFileName fileName <>))
            impFNames
  (defs', state'') <- do
    let (state'', mdefs') = runParser' pDefinitionBlock state'
    defs' <- liftEither $ first ParseError mdefs'
    return (defs', state'')
  defs'' <- do
    let (((), errs), defs'') = runState (runWriterT $ processDefs defs') defs
    case errs of
      [] -> return defs''
      _ -> throwError $ ProcessError errs
  doc <-
    liftEither $
      first ParseError $
        snd $ runParser' (pDocument defs'') state''
  return (defs'', doc)
