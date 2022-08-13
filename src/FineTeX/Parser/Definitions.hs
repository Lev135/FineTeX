{-# LANGUAGE RecordWildCards #-}

module FineTeX.Parser.Definitions where

import Control.Applicative (Alternative (..), optional)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.Parser.OptParser (OptParser, flagOpt, labelOpt, mkOptP, toParsec, (<??>), (<||>))
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils
import FineTeX.Utils (Box (..))
import Text.Megaparsec (MonadParsec (..), Parsec, choice, option, (<?>))
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (Word)

type Parser = Parsec Void Text

pImportFilenames :: Parser [Posed FilePath]
pImportFilenames =
  scn
    *> many
      ( inArgsEnvironment
          "Import"
          (\p _ -> T.unpack <$> p)
          (pStringLiteralL <?> "FilePath")
          (return ())
      )

pDefBlock :: Parser (DefBlock Posed)
pDefBlock = inEnvironment "Define" id (pDefModesBlock <|> pDefInModeBlock)

pDefModesBlock :: Parser (DefSubBlock Posed)
pDefModesBlock = inEnvironment "Modes" DefModeBlock $ do
  _name <- pIdentifierL
  return DefMode {..}

pDefInModeBlock :: Parser (DefSubBlock Posed)
pDefInModeBlock = L.indentBlock scn $ do
  name <- strLexeme "@In" *> pIdentifierL <?> "@In <ModeName>"
  return $
    L.IndentMany Nothing (pure . DefInModeBlock name) $
      choice [pDefEnvBlock name, pDefRuleBlock, pDefPrefBlock name, pDefInlBlock name]

pDefEnvBlock :: Posed Text -> Parser (DefInModeBlock Posed)
pDefEnvBlock mode = inEnvironment "Environments" DefEnvBlock $ do
  _name <- pIdentifierL
  _args <- pDefArgs
  strLexeme "="
  ((_begin, _end), _inner) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      inner <-
        option
          (NoVerb $ EnvNoVerb {_innerModeName = mode, _noPrefInside = False})
          ( Verb False <$ labelOpt "Verb"
              <||> Verb True <$ labelOpt "VerbIndent"
              <||> do
                inMode <- optional $ mkOptP "InnerMode" pIdentifierL
                _noPrefInside <- flagOpt "NoPrefInside"
                case (inMode, _noPrefInside) of
                  (Nothing, False) -> empty
                  _ ->
                    return . NoVerb $
                      EnvNoVerb {_innerModeName = fromMaybe mode inMode, _noPrefInside}
              <??> ["Verb", "VerbIndent", "InnerMode"]
          )
      return (beginEnd, inner)
  return DefEnvironment {..}

pDefRuleBlock :: Parser (DefInModeBlock Posed)
pDefRuleBlock = inEnvironment "Commands" DefRuleBlock $ do
  _match <- pPatMatchExp
  strLexeme "="
  _rule <- pRuleTerms
  return DefRule {..}

pDefPrefBlock :: Posed Text -> Parser (DefInModeBlock Posed)
pDefPrefBlock mode = inEnvironment "Prefs" DefPrefBlock $ do
  _name <- pPrefixL
  _args <- pDefArgs
  strLexeme "="
  ((_begin, _end), _pref, _suf, _sep, _innerModeName, _noPrefInside, _grouping, _oneLine) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      pref <- option [] (mkOptP "Pref" pRuleTerms)
      suf <- option [] (mkOptP "Suf" pRuleTerms)
      sep <- option [] (mkOptP "Sep" pRuleTerms)
      mode <- option mode (mkOptP "InnerMode" pIdentifierL)
      noPref <- flagOpt "NoPrefInside"
      grouping <- not <$> flagOpt "NoGroup"
      oneLine <- flagOpt "OneLine"
      return (beginEnd, pref, suf, sep, mode, noPref, grouping, oneLine)
  return DefPref {..}

pDefInlBlock :: Posed Text -> Parser (DefInModeBlock Posed)
pDefInlBlock mode = inEnvironment "Inlines" DefInlBlock $ do
  _borders <- (,) <$> pWordL <*> pWordL
  strLexeme "="
  (_begin, _end, _innerModeName) <- pOpt $ do
    (begin, end) <- pBeginEndOpt
    innerMode <- option mode (mkOptP "InnerMode" pIdentifierL)
    return (begin, end, innerMode)
  return DefInline {..}

inEnvironment :: Text -> ([a] -> b) -> Parser a -> Parser b
inEnvironment name f pel =
  L.indentBlock scn $
    atLexeme name $> L.IndentMany Nothing (pure . f) pel

inArgsEnvironment :: Text -> (args -> [a] -> b) -> Parser args -> Parser a -> Parser b
inArgsEnvironment name f pargs pel = L.indentBlock scn $ do
  atLexeme name
  args <- pargs
  return $ L.IndentMany Nothing (pure . f args) pel

pOpt :: OptParser a -> Parser a
pOpt = toParsec (unBox <$> optNameP) optArgsConsumer
  where
    optNameP = try (string "@" >> pIdentifierL) <?> "option name `@<name>`"
    optArgsConsumer = takeWhileP Nothing (`notElem` ['@', '\n', '\r', '%'])

{-}
ppPatMatchExp :: forall p. Box p => PatMatchExp p -> Text
ppPatMatchExp PatMatchExp {_behind, _current, _ahead} =
  T.unwords [toStr _behind, toStr _current, toStr _ahead]
  where
    toStr :: [PatMatchEl p] -> Text
    toStr = T.intercalate " " . map toStrEl

    toStrEl :: PatMatchEl p -> Text
    toStrEl (PatMatchEl var se) = case var of
      Just v -> "(" <> unBox v <> " : " <> sortToStr se <> ")"
      Nothing -> sortToStr se

    sortToStr :: SortExp p -> Text
    sortToStr = \case
      SEString s -> "\"" <> unBox s <> "\""
      SESpace -> "_"
      SESort sortName -> unBox sortName
      SEConcat s1 s2 -> brac s1 (sortToStr s1) <> " " <> brac s2 (sortToStr s2)
      SEOr s1 s2 -> sortToStr s1 <> " | " <> sortToStr s2

    brac (SEOr _ _) = \str -> "(" <> str <> ")"
    brac _ = id
-}
pPatMatchExp :: Parser (PatMatchExp Posed)
pPatMatchExp = do
  _behind <- option SEEmpty h
  _current <- many pPatMatchEl
  _ahead <- option SEEmpty h
  return $ PatMatchExp {..}
  where
    h =
      SESpace <$ strLexeme "_"
        <|> strLexeme "?"
          *> ( try pSortExp
                 <|> strLexeme "(" *> pSortExp <* strLexeme ")"
             )

pPatMatchEl :: Parser (PatMatchEl Posed)
pPatMatchEl = try pSimple <|> pBracet
  where
    pSimple = PatMatchEl Nothing <$> pSortExp
    pBracet = do
      strLexeme "("
      _var <- Just <$> pIdentifierL
      strLexeme ":"
      _sort <- pSortExp
      strLexeme ")"
      return PatMatchEl {..}

pSortExp :: Parser (SortExp Posed)
pSortExp = makeExprParser pTerm operators
  where
    pTerm :: Parser (SortExp Posed)
    pTerm =
      SEString <$> pStringLiteralL
        <|> SESpace <$ strLexeme "_"
        <|> SESort <$> pIdentifierL
        <|> strLexeme "(" *> pSortExp <* strLexeme ")"
    operators :: [[Operator Parser (SortExp Posed)]]
    operators =
      [ [InfixL (SEConcat <$ notFollowedBy (char '|'))],
        [InfixL (SEOr <$ strLexeme "|")]
      ]

pRuleTerms :: Parser [RuleTerm Posed]
pRuleTerms = many pRuleTerm

pRuleTerm :: Parser (RuleTerm Posed)
pRuleTerm =
  RTString <$> pStringLiteralL
    <|> RTVar <$> pIdentifierL
    <|> RTSpace <$ strLexeme "_"
    <|> RTRun <$> (strLexeme "!" *> optional pIdentifierL) <*> pBrackets
  where
    pBrackets = char '(' *> some pRuleTerm <* char ')' <|> (: []) <$> pRuleTerm

pBeginEndOpt :: OptParser ([RuleTerm Posed], [RuleTerm Posed])
pBeginEndOpt = option ([], []) $ texBEP <||> simpleBEP <??> ["@TexBeginEnd", "@Begin @End"]
  where
    texBEP = do
      texName <- mkOptP "TexBeginEnd" pStringLiteralL
      return ([RTString $ beg <$> texName], [RTString $ end <$> texName])
    beg n = "\\begin{" <> n <> "}"
    end n = "\\end{" <> n <> "}"
    simpleBEP = do
      begin <- option [] $ mkOptP "Begin" pRuleTerms
      end <- option [] $ mkOptP "End" pRuleTerms
      case (begin, end) of
        ([], []) -> empty
        _ -> return (begin, end)

pArgKind :: Parser (ArgKind Posed)
pArgKind =
  AKString <$ strLexeme "String"
    <|> AKSort <$> pSortExp
      <?> "argument kind 'String' or sort expression"

pDefArgs :: Parser [Argument Posed]
pDefArgs = many . label "argument `(<name> : <kind>)`" $ do
  try $ strLexeme "("
  _name <- pIdentifierL
  strLexeme ":"
  _kind <- pArgKind
  strLexeme ")"
  return Argument {..}
