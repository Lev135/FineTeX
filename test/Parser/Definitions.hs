module Parser.Definitions where

import Data.Void (Void)
import FineTeX.Parser.Definitions
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils (Posed (Posed))
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

pos a = Posed a undefined

parseAll p = MP.parse (p <* MP.eof) ""

spec :: SpecWith ()
spec = do
  describe "Combinators" $ do
    context "inEnvironment" $ do
      let parseWith p = parseAll (inEnvironment "Env" id p)
      context "char a" $ do
        let parse = parseWith (MP.char 'a')
        it "Empty" $
          parse `shouldFailOn` ""
        it "Empty @Env without eoln" $
          parse "@Env" `shouldParse` ""
        it "Empty @Env with eoln" $
          parse "@Env\n" `shouldParse` ""
        it "Incorrect indentation" $
          parse `shouldFailOn` "@Env\na"
        it "Ok" $
          parse "@Env\n a" `shouldParse` "a"
        it "Ok with eol" $
          parse "@Env\n  a\n" `shouldParse` "a"
        it "Ok with trailing spaces" $
          parse "@Env\n a   " `shouldParse` "a"
        it "Ok with empty lines" $
          parse "@Env\n\n a" `shouldParse` "a"
        it "Many" $
          parse "@Env\n a\n a" `shouldParse` "aa"
        it "Many with empty lines" $
          parse "@Env\n\n a\n   \n a" `shouldParse` "aa"
        it "Many with different indentation" $
          parse `shouldFailOn` "@Env\n a\n \n  a"

  describe "Parsing imports" $ do
    let parse = parseAll pImportFilenames
    it "Empty import block" $
      parse "" `shouldParse` []
    it "One import with in-dir path" $
      parse "@Import \"file\"" `shouldParse` [pos "file"]
    it "One relative path in subdir" $
      parse "@Import \"dir/file\"" `shouldParse` [pos "dir/file"]
    it "One relative windows-styled path in subdir" $
      parse "@Import \"dir\\file\"" `shouldParse` [pos "dir\\file"]
    it "Two imports" $
      parse "@Import \"imp1\"\n@Import \"imp2\"" `shouldParse` [pos "imp1", pos "imp2"]
    it "Empty lines and spaces" $
      parse "\n\n@Import   \"imp1\"    \n\n\n@Import \"imp2\" \n     \n" `shouldParse` [pos "imp1", pos "imp2"]
    it "Empty with empty lines" $
      parse "  \n\n   \n  " `shouldParse` []

-- describe "Parsing definitions" $ do
--   context "modes" $ do
--     let parse = parseAll pDefModesBlock
--     _
