{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Processor.Body where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import FineTeX.Parser.Syntax (WordOrSpace (..))
import FineTeX.Parser.Utils (Posed (..))
import FineTeX.Processor.Tokenizer (BlackWhiteSet (..), Token (..), TokenizeError (..), makeTokenizeMap, tokenize)
import GHC.Exts (IsList (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

instance s ~ Text => IsString (Posed s) where
  fromString str = Posed undefined (T.pack str)

instance IsString WordOrSpace where
  fromString str = ParWord (fromString str)

-- takeEL :: [DocElement Posed] -> [DocElement Posed]
-- takeEL = takeOutEmptyLines

instance IsList (BlackWhiteSet Char) where
  type Item (BlackWhiteSet Char) = Char
  fromList = WhiteSet . S.fromList
  toList = undefined

tok :: String -> ([BlackWhiteSet Char], [S.Set Char], [BlackWhiteSet Char]) -> Token String Char String
tok name (behind, body, ahead) = Token {name, body, behind, ahead, postProc = id}

spec :: SpecWith ()
spec = do
  {-  describe "Testing takeOutEmptyLines" $ do
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
  -}
  describe "Testing tokenizer" $ do
    let toks =
          M.fromList $
            map
              (\(name, tr) -> (name, tok name tr))
              [ ("a", ([], [['a']], [])),
                ("b", ([], [['b']], [])),
                ("c", ([], [['c']], [])),
                ("ab", ([], [['a'], ['b']], [])),
                ("a|b", ([], [['a', 'b']], [])),
                ("a(?b)", ([], [['a']], [WhiteSet ['b']]))
              ]
        t name = toks M.! name
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
      tokenize (m ["a|b"]) "ab" `shouldBe` Right ["a", "b"]
    it "Two char with or" $
      tokenize (m ["a|b", "c"]) "ac" `shouldBe` Right ["a", "c"]
    it "a, ab  vs  aba" $
      tokenize (m ["a", "ab"]) "aba" `shouldBe` Right ["ab", "a"]
    it "a, ab  vs  abaabab" $
      tokenize (m ["a", "ab"]) "abaabab" `shouldBe` Right ["ab", "a", "ab", "ab"]
    it "Char fail" $
      tokenize (m ["a"]) "x" `shouldBe` Left (NoWayTokenize 0 [])
    it "Char fail" $
      tokenize (m ["a", "a|b"]) "x" `shouldBe` Left (NoWayTokenize 0 [])
    it "Two chars fail" $
      tokenize (m ["a"]) "ax" `shouldBe` Left (NoWayTokenize 1 [("a", "a")])
    it "Many chars fail" $
      tokenize (m ["a"]) "aaax"
        `shouldBe` Left (NoWayTokenize 3 [("a", "a"), ("a", "a"), ("a", "a")])
    describe "Positive lookahead" $ do
      it "a(?b) vs a" $
        tokenize (m ["a(?b)"]) "a" `shouldBe` Right ["a"]
      it "a(?b) vs aa" $
        tokenize (m ["a(?b)"]) "aa" `shouldBe` Left (NoWayTokenize 0 [])
      it "a(?b) vs ab" $
        tokenize (m ["a(?b)"]) "ab" `shouldBe` Left (NoWayTokenize 1 [("a(?b)", "a")])
      it "a(?b), b vs ab" $
        tokenize (m ["a(?b)", "b"]) "ab" `shouldBe` Right ["a", "b"]
