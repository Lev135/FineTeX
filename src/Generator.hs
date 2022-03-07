{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Generator where

import Utils

import Text.Megaparsec(Parsec, MonadParsec (takeWhileP, label, takeWhile1P, try, notFollowedBy, lookAhead, eof), Pos, sepBy1, sepBy, unPos, (<?>), choice, optional, parse, errorBundlePretty, mkPos)
import Text.Megaparsec.Char ( char, space1, newline, letterChar, string )
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void(Void)
import Data.Text (Text, intercalate, replace, pack, unpack)
import qualified Data.Text as T
import Control.Monad (void, when, unless)
import Control.Applicative ( Alternative(empty, (<|>), some, many) )
import Text.Megaparsec.Char.Lexer (indentGuard)
import Data.Maybe (maybeToList, isJust, mapMaybe)
import Data.Bifunctor (Bifunctor(second))
import Data.Char (isLetter, isSpace)
import Data.List (intersperse)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void . some $ char ' ' <|> char '\t') empty empty

scn :: Parser ()
scn = L.space space1 empty empty

indentLevel :: Parser Pos
indentLevel = sc *> L.indentLevel

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser Text
identifier = lexeme (takeWhileP Nothing (\ch -> isLetter ch || ch == '-'))
    <?> "Identifier"

strLexeme :: Text -> Parser Text
strLexeme = lexeme . string

atLexeme :: Text -> Parser Text
atLexeme = strLexeme . ("@" <>)

pStringBetween :: Char -> Parser Text
pStringBetween ch = chP *> takeWhileP Nothing (/= ch) <* chP
    where chP = string $ T.singleton ch

pVerbString :: Parser Text
pVerbString = lexeme (choice (pStringBetween <$> ['`', '"', '\''])) <?> "Verb string"

data Environment = Environment {
                name, begin, end    :: Text,
                innerMath           :: Bool,
                pref                :: Maybe Text,
                group               :: Maybe Text,
                sep                 :: Maybe Text
            }
    deriving Show
data Command  = Command {
                name, val           :: Text
            }
    deriving Show

-- instance Show Environment where
--     show Environment{name} = unpack name

data Definition
    = DefE  Environment
    | DefMC Command
    | DefC  Command
    deriving Show

pDefinitionBlock :: Parser [Definition]
pDefinitionBlock = L.nonIndented scn . L.indentBlock scn $ do
    atLexeme "Define"
    return $ L.IndentMany Nothing (return . concat)
         $  map DefE  <$> pEnvsDef
        <|> map DefC  <$> pCmdsDef
        <|> map DefMC <$> pMathCmdsDef

data Option
    = OptInnerMath
    | OptPref  Text
    | OptGroup Text
    | OptSep   Text

pMathCmdsDef :: Parser [Command]
pMathCmdsDef = L.indentBlock scn $ do
    atLexeme "MathCommands"
    return $ L.IndentSome Nothing return pCmdDef

pCmdsDef :: Parser [Command]
pCmdsDef = L.indentBlock scn $ do
    atLexeme "Commands"
    return $ L.IndentSome Nothing return pCmdDef


pCmdDef :: Parser Command
pCmdDef = do
    name      <- pVerbString
    strLexeme "="
    val       <- pVerbString
    math      <- isJust <$> optional (atLexeme "Math")
    return Command{ name, val }

pEnvsDef :: Parser [Environment]
pEnvsDef = L.indentBlock scn $ do
    atLexeme "Environments"
    return $ L.IndentSome Nothing return $ do
        name      <- identifier
        strLexeme "="
        (begin, end) <- pTeX <|> pBeginEnd
        options <- many pOption
        let h f = case mapMaybe f options of
                        []  -> return Nothing
                        [a] -> return $ Just a
                        _   -> fail "Option occures twice"
        innerMath <- isJust <$> h optInnerMath
        pref      <- h optPref
        group     <- h optGroup
        sep       <- h optSep
        return Environment{ name, begin, end, innerMath, pref, group, sep }
    where
        pTeX = do
            atLexeme "TeX"
            texI <- pVerbString
            return ("\\begin{" <> texI <> "}\n", "\\end{"   <> texI <> "}\n")
        pBeginEnd = (,) <$> pVerbString <*> pVerbString
        pOption = choice [
                OptInnerMath <$   atLexeme "Math",
                OptPref      <$> (atLexeme "Pref"  *> pVerbString),
                OptGroup     <$> (atLexeme "Group" *> identifier),
                OptSep       <$> (atLexeme "Sep"   *> pVerbString)
            ]
        optInnerMath OptInnerMath = Just ()
        optInnerMath _            = Nothing
        optPref (OptPref t) = Just t
        optPref _           = Nothing
        optGroup (OptGroup t) = Just t
        optGroup _            = Nothing
        optSep   (OptSep   t) = Just t
        optSep   _            = Nothing
data Definitions = Definitions {
        envs         :: [(Text, Environment)],
        cmds         :: [(Text, Command)],
        mathCmds     :: [(Text, Command)],
        prefLineEnvs :: [(Text, Environment)]
    }

instance Semigroup Definitions where
    (Definitions a b c d) <> (Definitions a' b' c' d')
        = Definitions (a <> a') (b <> b') (c <> c') (d <> d')
instance Monoid Definitions where
    mempty = Definitions [] [] [] []

processDefs :: [Definition] -> Definitions
processDefs = mconcat . map processDef

processDef :: Definition -> Definitions
processDef (DefE env@Environment{name, begin, end, innerMath, pref, group})
    = Definitions {
            envs = [(name, env)], cmds = [], mathCmds = [],
            prefLineEnvs = (, env) <$> maybeToList pref
        }
processDef (DefC cmd@Command{name})
    = Definitions {
            envs = [], cmds = [(name, cmd)], mathCmds = [],
            prefLineEnvs = []
        }
processDef (DefMC cmd@Command{name})
    = Definitions {
            envs = [], cmds = [], mathCmds = [(name, cmd)],
            prefLineEnvs = []
        }

data DocElement
    = DocParagraph     [[ParEl]]
    | DocEnvironment   Environment [DocElement]
    | DocString        Text
    deriving Show
-- instance Show DocElement where
--     show (DocParagraph els _) = unlines . map (unwords . map show) $ els
--     show (DocEnvironment env els _) = unlines (show env : (show <$> els))

data ParEl
    = ParText Text
    | ParFormula Text
    deriving Show
-- instance Show ParEl where
--     show (ParText    t) = unpack t
--     show (ParFormula t) = "`" ++ unpack t ++ "`"

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

texNewL :: Bool -> Text
texNewL True  = "\n"
texNewL False = mempty

texParEl :: Definitions -> Bool -> ParEl -> Text
texParEl _    False (ParText    t) = t
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Text
texMath Definitions{mathCmds} = foldr (.) id fs
    where fs = map (uncurry replace . second ((<>" ") . val)) mathCmds

tabWidth :: Int
tabWidth = 2

incIndent :: Pos -> Pos
incIndent = mkPos . (+ tabWidth) . unPos

pDocument :: Definitions -> Parser [DocElement]
pDocument defs = L.nonIndented scn (pElements defs $ mkPos 1) <* scn

pElements :: Definitions -> Pos -> Parser [DocElement]
pElements defs ind = pElement defs ind `listSepBy_` try (notFollowedBy eof >> checkIndent >> pEmptyLine)
    where
        checkIndent = do
            ind' <- indentLevel
            unless (ind' >= ind)
                $ fail $ "Incorrect indentation (got "
                    <> show (unPos ind')
                    <> " should be greater or equal "
                    <> show (unPos ind)

pElement :: Definitions -> Pos -> Parser [DocElement]
pElement defs ind
     =  pPrefLineEnvironment    defs ind
    <|> (:[]) <$> pEnvironment  defs ind
    <|> (:[]) <$> pParagraph    defs ind

pPrefLineEnvironment :: Definitions -> Pos -> Parser [DocElement]
pPrefLineEnvironment defs@Definitions{prefLineEnvs, envs} ind = do
    try $ indentGuard sc EQ (incIndent ind)
    choice (try . uncurry mkP <$> prefLineEnvs)
    where
        mkP' :: Text -> Environment -> Parser [DocElement]
        mkP' pref env@Environment{sep} = do
            let pPref = indentGuard sc EQ (incIndent ind) *> string (pref <> " ")
            pPref
            let pEl = do
                    pos'' <- indentLevel
                    DocEnvironment env <$> pElements defs  pos''
            els <- pEl `sepBy` try pPref
            case sep of
                Nothing -> return els
                Just s  -> return $ intersperse (DocString s) els

        mkP :: Text -> Environment -> Parser [DocElement]
        mkP pref env@Environment{group} = do
            els <- mkP' pref env
            case group >>= flip lookup envs of
                Nothing     -> return $ els
                Just grEnv  -> return [DocEnvironment grEnv els]


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
        string "@" *> identifier <* sc <* newline
    case lookup name envs of
        Nothing  -> fail $ "Undefined environment " ++ show name
        Just env -> DocEnvironment env <$> pElements defs (incIndent ind)

pEmptyLine :: Parser [DocElement]
pEmptyLine = h <$> (optional . try $ sc <* newline <* scn)
    where
        h Nothing  = []
        h (Just _) = [DocString "\n"]
pFile :: Parser (Definitions, [DocElement])
pFile = do
    defs <- processDefs <$> pDefinitionBlock
    doc  <- pDocument defs
    return (defs, doc)
