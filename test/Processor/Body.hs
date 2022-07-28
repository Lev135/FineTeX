{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Processor.Body where

import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax (DocElement (..), EnvBody (NoVerbBody), WordOrSpace (..))
import FineTeX.Parser.Utils (Posed (..))
import FineTeX.Processor.Body
import FineTeX.Processor.Tokenizer (BlackWhiteSet (..), Token (..), makeTokenizeMap, tokenize)
import GHC.Exts (IsList (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

instance s ~ Text => IsString (Posed s) where
  fromString str = Posed (T.pack str) undefined

instance IsString (WordOrSpace Posed) where
  fromString str = ParWord (fromString str)

takeEL :: [DocElement Posed] -> [DocElement Posed]
takeEL = takeOutEmptyLines

instance IsList (BlackWhiteSet Char) where
  type Item (BlackWhiteSet Char) = Char
  fromList = WhiteSet . S.fromList
  toList = undefined

tok :: String -> ([BlackWhiteSet Char], [S.Set Char], [BlackWhiteSet Char]) -> Token String Char
tok name (behind, body, ahead) = Token {name, body, behind, ahead}

spec :: SpecWith ()
spec = do
  describe "Testing takeOutEmptyLines" $ do
    let el = DocEmptyLine
        par = DocParagraph [[]]
        env = DocEnvironment "Env" [] . NoVerbBody
    it "Empty" $
      takeEL [] `shouldBe` []
    it "EL" $
      takeEL [el] `shouldBe` [el]
    it "Par, EL, Par" $
      takeEL [par, el, par] `shouldBe` [par, el, par]
    it "Env[]" $
      takeEL [env []] `shouldBe` [env []]
    it "Env[EL]" $
      takeEL [env [el]] `shouldBe` [env [], el]
    it "Env[EL], EL" $
      takeEL [env [el], el] `shouldBe` [env [], el]
    it "Env[Env[EL]]" $
      takeEL [env [env [el]]] `shouldBe` [env [env []], el]
    it "Env[Par, EL, Par]" $
      takeEL [env [par, el, par]] `shouldBe` [env [par, el, par]]
    it "Env[Par, EL, Par, EL]" $
      takeEL [env [par, el, par, el]] `shouldBe` [env [par, el, par], el]
    it "Env[Par, EL, Env [EL]]" $
      takeEL [env [par, el, env [el]]] `shouldBe` [env [par, el, env []], el]
    it "Env[Par, EL], Par" $
      takeEL [env [par, el], par] `shouldBe` [env [par], el, par]
    it "Env[Par, EL], Env [EL, par]" $
      takeEL [env [par, el], env [el, par]] `shouldBe` [env [par], el, env [el, par]]
  describe "Testing tokenizer" $ do
    let toks =
          [ ("a", ([], [['a']], [])),
            ("b", ([], [['b']], [])),
            ("c", ([], [['c']], [])),
            ("ab", ([], [['a'], ['b']], [])),
            ("a|b", ([], [['a', 'b']], []))
          ]
        t name = case lookup name toks of
          Nothing -> undefined
          (Just tr) -> tok name tr
        m names = makeTokenizeMap (t <$> names)
    it "Empty" $
      tokenize (m []) "" `shouldBe` Right []
    it "Empty" $
      tokenize (m ["a"]) "" `shouldBe` Right []
    it "Empty" $
      tokenize (m ["a", "b"]) "" `shouldBe` Right []
    it "Char" $
      tokenize (m ["a", "b"]) "a" `shouldBe` Right ["a"]
    it "Two chars" $
      tokenize (m ["a", "b"]) "aa" `shouldBe` Right ["a", "a"]
    it "Two chars" $
      tokenize (m ["a", "b"]) "ab" `shouldBe` Right ["a", "b"]
    it "Two char with or" $
      tokenize (m ["a|b"]) "ab" `shouldBe` Right ["a|b", "a|b"]
    it "Two char with or" $
      tokenize (m ["a|b", "c"]) "ac" `shouldBe` Right ["a|b", "c"]
    it "a, ab  vs  aba" $
      tokenize (m ["a", "ab"]) "aba" `shouldBe` Right ["ab", "a"]
    it "a, ab  vs  abaabab" $
      tokenize (m ["a", "ab"]) "abaabab" `shouldBe` Right ["ab", "a", "ab", "ab"]
    it "Char fail" $
      tokenize (m ["a"]) "x" `shouldBe` Left [[]]
    it "Char fail" $
      tokenize (m ["a", "a|b"]) "x" `shouldBe` Left [[]]
    it "Two chars fail" $
      tokenize (m ["a"]) "ax" `shouldBe` Left [["a"]]
    it "Many chars fail" $
      tokenize (m ["a"]) "aaax" `shouldBe` Left [["a", "a", "a"]]
