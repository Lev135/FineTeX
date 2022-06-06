module Processor
  ( processDoc,
    ErrorType,
    Error,
    ErrorM,
    prettyError,
  )
where

import Control.Monad.Writer (Writer, tell)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict, toStrict)
import Parser
  ( Command (val),
    Definitions (Definitions, mathCmds),
    DocElement (..),
    Environment (Environment, innerMath),
    ParEl (..),
    ParWord,
    Pos,
    Pref (Pref, innerMath),
  )
import Text.Megaparsec (SourcePos (..), unPos)
import Text.Replace (Trie, mapToTrie, replaceWithTrie, text'fromText)

newtype Tries = Tries
  { mathCmdsTrie :: Trie
  }

data ErrorType
  = UnexpectedUnicodeSymbol Char
  | InlineMathInMathMode

prettyEType :: ErrorType -> Text
prettyEType (UnexpectedUnicodeSymbol ch) =
  "Unexpected unicode symbol: '" <> T.singleton ch <> "'"
prettyEType InlineMathInMathMode =
  "Unexpected inline formula in math mode"

data Error = Error ErrorType Pos

prettyError :: [Text] -> Error -> Text
prettyError lines (Error t (b, e)) =
  T.unlines
    [ T.concat (T.pack <$> [file, ":", show line, ":", show col, ": "])
        <> prettyEType t,
      lines !! (line - 1),
      mask
    ]
  where
    file = sourceName b
    line = unPos $ sourceLine b
    col = unPos $ sourceColumn b
    col' = unPos $ sourceColumn e
    mask =
      T.concat
        [ repl (col - 1) ' ',
          T.singleton '^',
          repl (col' - col - 1) '~',
          T.singleton '^'
        ]
    repl n ch = T.replicate n (T.singleton ch)

type ErrorM = Writer [Error]

processDoc :: Definitions -> [DocElement] -> ErrorM [DocElement]
processDoc Definitions {mathCmds} = mapM $ processDocElement tries False
  where
    tries =
      Tries
        { mathCmdsTrie = mapToTrie . M.map ((<> " ") . val) . M.mapKeys text'fromText $ mathCmds
        }

processDocElement :: Tries -> Bool -> DocElement -> ErrorM DocElement
processDocElement tries math (DocParagraph els) =
  DocParagraph
    <$> (mapM . mapM) (processParEl tries math) els
processDocElement tries math (DocEnvironment env@Environment {innerMath} argvs els) =
  DocEnvironment env argvs
    <$> mapM (processDocElement tries (math || innerMath)) els
processDocElement tries math (DocPrefGroup pref@Pref {innerMath} els) =
  DocPrefGroup pref
    <$> (mapM . mapM) (processDocElement tries (math || innerMath)) els
processDocElement _ _ DocEmptyLine = return DocEmptyLine
processDocElement _ _ v@(DocVerb _ _) = return v

processParEl :: Tries -> Bool -> ParEl -> ErrorM ParEl
processParEl tries math el = case (math, el) of
  (False, ParText ws) ->
    el <$ mapM checkAscii ws
  (False, ParFormula ws) ->
    let ws' = first (texMath tries) <$> ws
     in ParFormula ws' <$ mapM checkAscii ws'
  (True, ParText ws) ->
    let ws' = first (texMath tries) <$> ws
     in ParText ws' <$ mapM checkAscii ws'
  (True, ParFormula ws) -> do
    -- TODO: Make it without possible exceptions!!!!!
    let (_, (p1, _)) = head ws
        (_, (_, p2)) = last ws
    tell [Error InlineMathInMathMode (p1, p2)]
    return el

isCyrillic :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

checkAscii :: ParWord -> ErrorM ()
checkAscii (t, p) = case T.find (\ch -> not (isAscii ch || isCyrillic ch)) t of
  Nothing -> return ()
  Just c -> tell [Error (UnexpectedUnicodeSymbol c) p]

texMath :: Tries -> Text -> Text
texMath Tries {mathCmdsTrie} = rmSpaces . toStrict . replaceWithTrie mathCmdsTrie . fromStrict
  where
    rmSpaces :: Text -> Text
    rmSpaces = T.foldr h ""
      where
        h ' ' xs | T.null xs || not (isAlphaNum $ T.head xs) = xs
        h x xs = T.cons x xs
