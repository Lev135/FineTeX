module Main where

import qualified Parser.Body
import qualified Parser.Definitions
import qualified Processor.Body
import Test.Hspec

main = hspec $ do
  Parser.Definitions.spec
  Parser.Body.spec
  Processor.Body.spec
