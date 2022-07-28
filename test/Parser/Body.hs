{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser.Body where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalState, evalStateT)
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import FineTeX.FineTeX (Definitions, parseDefSource, runParseT)
import FineTeX.Parser.Body
import FineTeX.Parser.Definitions (Parser)
import FineTeX.Parser.Syntax (ArgVal (AVString), DocElement (DocEmptyLine, DocEnvironment, DocParagraph), EnvBody (NoVerbBody, VerbBody), ParEl (ParInline, ParText), WordOrSpace (ParSpace, ParWord))
import FineTeX.Parser.Utils
import FineTeX.Utils (Box (..))
import Test.Hspec (context, describe, it)
import Test.Hspec.Megaparsec (failsLeaving, initialState, shouldFailOn, shouldParse, succeedsLeaving)
import Text.Megaparsec (MonadParsec (getParserState), errorBundlePretty, some)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (string)
import Text.RawString.QQ (r)

defs :: Definitions
defs = case runParseT $ parseDefSource (\_ _ -> Just $ Right (txt, undefined)) undefined "" of
  Just (Right defs) -> defs
  Just (Left e) -> error $ unpack e
  _ -> undefined
  where
    txt =
      [r|
@Define
  @Modes
    Normal
    Math
  @In Normal
    @Environments
      Env            = 
      NoPrefEnv      = @NoPrefInside
      MathEnv        = @InnerMode Math
      VerbEnv        = @Verb
      VerbIndEnv     = @VerbIndent
      ArgEnv (x : String) =

    @Prefs
      >              = @TexBeginEnd "align*"    @InnerMode Math  @Sep "\\" @NoPrefInside
      -              = @TexBeginEnd "itemize"   @Pref "\item"
      #              = @TexBeginEnd "enumerate" @Pref "\item"
    @Inlines
      ` ` = @Begin "$" @End "$" @InnerMode Math

  @In Math
    @Commands
      <=           = "\leq"
      >=           = "\geq"
      !=           = "\neq"
    @Inlines
      ?? ?? = @Begin "\text{" @End "}" @InnerMode Normal
|]

instance s ~ Text => IsString (Posed s) where
  fromString str = Posed (T.pack str) undefined

instance IsString (WordOrSpace Posed) where
  fromString str = ParWord (fromString str)

run :: _ -> Parser _
run = flip evalStateT defaultState . flip runReaderT defs

from :: Text -> Text -> MP.State Text Void
from str pref = case MP.parse (string pref *> getParserState :: Parser _) "" str of
  Left peb ->
    error $
      unlines
        [ "Incorrect test case: ",
          "  Prefix: " <> show pref,
          "  String: " <> show str,
          "Prefix consumer output: ",
          "  " <> errorBundlePretty peb
        ]
  Right st -> st

parsePart :: MP.Parsec e s a -> MP.State s e -> (MP.State s e, Either (MP.ParseErrorBundle s e) a)
parsePart = MP.runParser'

parseAll :: (MP.Stream s, Ord e) => MP.Parsec e s a -> s -> Either (MP.ParseErrorBundle s e) a
parseAll p = MP.parse (p <* MP.eof) ""

spec = do
  describe "Parsing body parts" $ do
    context "Parsing ParText" $ do
      let parse = parseAll (run pParText)
      it "Empty" $
        parse `shouldFailOn` ""
      it "One word" $
        parse "Word" `shouldParse` ParText ["Word"]
      it "Space" $
        parse " " `shouldParse` ParText [__]
      it "Many spaces" $
        parse "    " `shouldParse` ParText [__]
      it "Many words" $
        parse "One  two    three  " `shouldParse` ParText ["One", __, "two", __, "three", __]
      it "Starting with space" $
        parse "  One    two   " `shouldParse` ParText [__, "One", __, "two", __]
    context "Parsing ParEls" $ do
      let parse = parseAll $ run (some pParEl)
      it "Empty" $
        parse `shouldFailOn` ""
      it "Only text" $
        parse "  Some  text" `shouldParse` [ParText [__, "Some", __, "text"]]
      it "Empty inline" $
        parse "``" `shouldParse` [ParInline "`" []]
      it "Inline with empty inline" $
        parse "`????`" `shouldParse` [ParInline "`" [ParInline "??" []]]
      it "One inline" $
        parse "`a + b`" `shouldParse` [ParInline "`" [ParText ["a", __, "+", __, "b"]]]
      it "Inline and text" $
        parse "sum `a` plus `b`"
          `shouldParse` [ ParText ["sum", __],
                          ParInline "`" [ParText ["a"]],
                          ParText [__, "plus", __],
                          ParInline "`" [ParText ["b"]]
                        ]
      it "Inline spaces" $
        parse "a`b`c `d `` f` g"
          `shouldParse` [ ParText ["a"],
                          ParInline "`" [ParText ["b"]],
                          ParText ["c", __],
                          ParInline "`" [ParText ["d", __]],
                          ParInline "`" [ParText [__, "f"]],
                          ParText [__, "g"]
                        ]
      it "Complex inline" $
        parse "a`b??c`` `e`??`"
          `shouldParse` [ ParText ["a"],
                          ParInline
                            "`"
                            [ ParText ["b"],
                              ParInline "??" [ParText ["c"], ParInline "`" [], ParText [__], ParInline "`" [ParText ["e"]]]
                            ]
                        ]
      let parse' = parsePart (run $ some pParEl)
      it "Two lines" $
        parse' ("abc `d`\nef" `from` "") `succeedsLeaving` "\nef"
      it "Unclosed inline" $
        parse' ("abc `d\nef`" `from` "") `failsLeaving` "\nef`"

    context "Parsing paragraph without inlines" $ do
      let parse = parseAll (run pParagraph)
      it "Empty" $
        parse `shouldFailOn` ""
      it "One line" $
        parse "Some   text   here  "
          `shouldParse` DocParagraph [[ParText ["Some", __, "text", __, "here", __]]]
      it "Line with eol" $
        parse "Some text\n" `shouldParse` DocParagraph [[ParText ["Some", __, "text"]]]
      it "Two lines" $
        parse "First line   \nSecond   line"
          `shouldParse` DocParagraph [[ParText ["First", __, "line", __]], [ParText ["Second", __, "line"]]]
      let parse' = parsePart (run pParagraph)
      it "Positive indentation" $
        parse' ("First line\n  Second line" `from` "") `succeedsLeaving` "Second line"
      it "Negative indentation" $
        parse' ("  First line\nSecond line" `from` "  ") `succeedsLeaving` "Second line"
      it "Environment" $
        parse' ("First line\n@Second line" `from` "") `succeedsLeaving` "@Second line"

    context "Parsing environment" $ do
      let parse = parseAll (run pEnvironment)
      let parse' = parsePart (run pEnvironment)
      let envs = ["Env", "NoPrefEnv", "MathEnv", "VerbEnv", "VerbIndEnv"]
      let body env
            | env == "VerbEnv" = VerbBody False []
            | env == "VerbIndEnv" = VerbBody True []
            | otherwise = NoVerbBody []
      it "Empty" $
        parse `shouldFailOn` ""
      sequence_
        [ it ("Empty " <> T.unpack aenv) $
            parse aenv `shouldParse` DocEnvironment env [] (body env)
          | env <- envs,
            let aenv = "@" <> unBox env
        ]
      it "Empty ArgEnv without argument" $
        parse `shouldFailOn` "@ArgEnv"
      it "Empty ArgEnv with argument" $
        parse "@ArgEnv \"tmp\"" `shouldParse` DocEnvironment "ArgEnv" [AVString "tmp"] (NoVerbBody [])
      sequence_
        [ it ("Empty " <> T.unpack aenv <> " with eoln") $
            parse aenv `shouldParse` DocEnvironment env [] (body env)
          | env <- envs,
            let aenv = "@" <> unBox env <> "\n"
        ]
      it "Empty ArgEnv without argument with eoln" $
        parse `shouldFailOn` "@ArgEnv\n"
      it "Empty ArgEnv with argument with eoln" $
        parse "@ArgEnv \"tmp\"\n" `shouldParse` DocEnvironment "ArgEnv" [AVString "tmp"] (NoVerbBody [])
      it "Env with paragraph" $
        parse "@Env\n  Some text"
          `shouldParse` DocEnvironment "Env" [] (NoVerbBody [DocParagraph [[ParText ["Some", __, "text"]]]])
      it "Env with empty line" $
        parse "@Env\n\n" `shouldParse` DocEnvironment "Env" [] (NoVerbBody [DocEmptyLine])
      it "Env two paragraphs" $
        parse "@Env\n  First\n\n  Second"
          `shouldParse` DocEnvironment
            "Env"
            []
            ( NoVerbBody [DocParagraph [[ParText ["First"]]], DocEmptyLine, DocParagraph [[ParText ["Second"]]]]
            )
      it "Env starting from empty line" $
        -- TODO: Add checking empty lines for other kinds of environments
        parse "@Env\n\n  a\n  b"
          `shouldParse` DocEnvironment
            "Env"
            []
            ( NoVerbBody
                [DocEmptyLine, DocParagraph [[ParText ["a"]], [ParText ["b"]]]]
            )
      it "Env with unexpected pref" $
        parse `shouldFailOn` "@Env\n  text\n    tmp"
      it "Env stops" $
        parse' ("@Env\n a\n\nb\n" `from` "") `succeedsLeaving` "b\n"
      it "Indented Env stops" $
        parse' ("  @Env\n   a\n\n  b\n" `from` "  ") `succeedsLeaving` "b\n"
      it "Indented Env stops" $
        parse' ("  @Env\n   a\n\n b\n" `from` "  ") `succeedsLeaving` "b\n"
      it "NoPrefEnv with unexpected pref" $
        parse "@NoPrefEnv\n  text\n    tmp"
          `shouldParse` DocEnvironment
            "NoPrefEnv"
            []
            ( NoVerbBody [DocParagraph [[ParText ["text"]]], DocParagraph [[ParText ["tmp"]]]]
            )
      it "MathEnv with inline" $
        parse "@MathEnv\n  ?? ??"
          `shouldParse` DocEnvironment
            "MathEnv"
            []
            ( NoVerbBody [DocParagraph [[ParInline "??" [ParText [__]]]]]
            )
      it "VerbEnv with many lines" $
        parse "@VerbEnv\n  a\n   b\n  c\n  d\n"
          `shouldParse` DocEnvironment "VerbEnv" [] (VerbBody False ["a", " b", "c", "d"])
      it "VerbEnv with empty line" $
        parse "@VerbEnv\n\n\n" `shouldParse` DocEnvironment "VerbEnv" [] (VerbBody False ["", ""])
      it "VerbEnv starting with empty line" $
        parse "@VerbEnv\n\n  a\n" `shouldParse` DocEnvironment "VerbEnv" [] (VerbBody False ["", "a"])
      it "VerbEnv with decrising indent" $
        parse "@VerbEnv\n   a\n  b\n c" `shouldParse` DocEnvironment "VerbEnv" [] (VerbBody False ["  a", " b", "c"])
      it "VerbEnv stops" $
        parse' ("@VerbEnv\n a\n\nb\n" `from` "") `succeedsLeaving` "b\n"
      it "Indented VerbEnv stops" $
        parse' ("  @VerbEnv\n   a\n\n  b\n" `from` "  ") `succeedsLeaving` "b\n"
      it "Indented VerbEnv stops" $
        parse' ("  @VerbEnv\n   a\n\n b\n" `from` "  ") `succeedsLeaving` "b\n"
  where
    __ = ParSpace
