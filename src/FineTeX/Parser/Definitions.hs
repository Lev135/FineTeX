{-# LANGUAGE RecordWildCards #-}

module FineTeX.Parser.Definitions where

import Control.Applicative (Alternative (..), optional)
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
import Text.Megaparsec.Char (string)
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
      choice [pDefEnvBlock name, pDefCmdBlock, pDefPrefBlock name, pDefInlBlock name]

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

pDefCmdBlock :: Parser (DefInModeBlock Posed)
pDefCmdBlock = inEnvironment "Commands" DefCmdBlock $ do
  _name <- pStringLiteralL
  strLexeme "="
  _val <- pWords
  return DefCommand {..}

pDefPrefBlock :: Posed Text -> Parser (DefInModeBlock Posed)
pDefPrefBlock mode = inEnvironment "Prefs" DefPrefBlock $ do
  _name <- pPrefixL
  _args <- pDefArgs
  strLexeme "="
  ((_begin, _end), _pref, _suf, _sep, _innerModeName, _noPrefInside, _grouping, _oneLine) <-
    pOpt $ do
      beginEnd <- pBeginEndOpt
      pref <- option [] (mkOptP "Pref" pWords)
      suf <- option [] (mkOptP "Suf" pWords)
      sep <- option [] (mkOptP "Sep" pWords)
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

pWords :: Parser [Word Posed]
pWords = many pWord

pWord :: Parser (Word Posed)
pWord =
  choice
    [ WSpace <$ strLexeme "_",
      WString <$> pStringLiteralL,
      WWord <$> pWordL
    ]

pBeginEndOpt :: OptParser ([Word Posed], [Word Posed])
pBeginEndOpt = option ([], []) $ texBEP <||> simpleBEP <??> ["@TexBeginEnd", "@Begin @End"]
  where
    texBEP :: OptParser ([Word Posed], [Word Posed])
    texBEP = do
      texName <- mkOptP "TexBeginEnd" pStringLiteralL
      return ([WString $ beg <$> texName], [WString $ end <$> texName])
    beg n = "\\begin{" <> n <> "}"
    end n = "\\end{" <> n <> "}"
    simpleBEP = do
      begin <- option [] $ mkOptP "Begin" pWords
      end <- option [] $ mkOptP "End" pWords
      case (begin, end) of
        ([], []) -> empty
        _ -> return (begin, end)

pArgKind :: Parser ArgKind
pArgKind =
  AKString <$ strLexeme "String"
    <|> AKWord <$ strLexeme "Word"
      <?> "argument type `String` | `Word`"

pDefArgs :: Parser [Argument Posed]
pDefArgs = many . label "argument `(<name> : <type>)`" $ do
  try $ strLexeme "("
  _name <- pIdentifierL
  strLexeme ":"
  _kind <- withPos pArgKind
  strLexeme ")"
  return Argument {..}
