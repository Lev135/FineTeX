module Processor
  ( processDoc,
    ProcessErrorType,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.Writer (Writer, tell)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Void (Void)
import Parser
  ( ArgType (..),
    ArgV (ArgVMath),
    Argument (..),
    Command (..),
    Definitions (..),
    DocElement (..),
    Environment (..),
    Inline (..),
    ParEl (..),
    ParWord,
    Pref (..),
  )
import Text.Replace (Trie, mapToTrie, replaceWithTrie, text'fromText)
import Utils (Box (unBox), Error (SimpleErr), PrettyErrType (..))

newtype Tries = Tries
  { mathCmdsTrie :: Trie
  }

data ProcessErrorType
  = UnexpectedUnicodeSymbol Char
  | InlineMathInMathMode
  deriving (Show)

instance PrettyErrType ProcessErrorType where
  prettyErrType (UnexpectedUnicodeSymbol ch) =
    "Unexpected unicode symbol: '" <> T.singleton ch <> "'"
  prettyErrType InlineMathInMathMode =
    "Unexpected inline formula in math mode"

type ErrorM = Writer [Error ProcessErrorType Void]

processDoc :: Box p => Definitions p -> [DocElement] -> ErrorM [DocElement]
processDoc Definitions {mathCmds} = mapM $ processDocElement tries False
  where
    tries =
      Tries
        { mathCmdsTrie =
            mapToTrie
              . M.map ((<> " ") . val)
              . M.mapKeys text'fromText
              . M.map unBox
              $ mathCmds
        }

processMathArgs :: Tries -> [Argument] -> [ArgV] -> ErrorM [ArgV]
processMathArgs tries = zipWithM h
  where
    h :: Argument -> ArgV -> ErrorM ArgV
    h arg argv = case atype arg of
      ArgMath -> case argv of
        ArgVMath ws -> ArgVMath <$> sequence (checkAscii . first (texMath tries) <$> ws)
        _ -> error "Arg type and value mismatch"
      ArgString -> return argv

processDocElement :: Tries -> Bool -> DocElement -> ErrorM DocElement
processDocElement tries math (DocParagraph els) =
  DocParagraph
    <$> (mapM . mapM) (processParEl tries math) els
processDocElement tries math (DocEnvironment env@Environment {innerMath, args} argvs els) = do
  args' <- processMathArgs tries args argvs
  els' <- mapM (processDocElement tries (math || innerMath)) els
  return $ DocEnvironment env args' els'
processDocElement tries math (DocPrefGroup pref@Pref {innerMath, args} els) = do
  DocPrefGroup pref <$> mapM fEl els
  where
    fEl (argvs, body) = (,) <$> fArgs argvs <*> fBody body
    fBody = mapM $ processDocElement tries (math || innerMath)
    fArgs = processMathArgs tries args
processDocElement _ _ DocEmptyLine = return DocEmptyLine
processDocElement _ _ v@(DocVerb _ _) = return v

processParEl :: Tries -> Bool -> ParEl -> ErrorM ParEl
processParEl tries math el = case (math, el) of
  (False, ParText ws) ->
    el <$ mapM checkAscii ws
  (False, ParInline inl@Inline {innerMath} ws) ->
    let ws' =
          if innerMath
            then first (texMath tries) <$> ws
            else ws
     in ParInline inl <$> mapM checkAscii ws'
  (True, ParText ws) ->
    let ws' = first (texMath tries) <$> ws
     in ParText <$> mapM checkAscii ws'
  (True, ParInline Inline {} ws) -> do
    -- TODO: Make it without possible exceptions!!!!!
    let (_, (p1, _)) = head ws
        (_, (_, p2)) = last ws
    tell [SimpleErr InlineMathInMathMode (p1, p2)]
    return el

isCyrillic :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

checkAscii :: ParWord -> ErrorM ParWord
checkAscii w@(t, p) =
  w <$ case T.find (\ch -> not (isAscii ch || isCyrillic ch)) t of
    Nothing -> return ()
    Just c -> tell [SimpleErr (UnexpectedUnicodeSymbol c) p]

texMath :: Tries -> Text -> Text
texMath Tries {mathCmdsTrie} = rmSpaces . toStrict . replaceWithTrie mathCmdsTrie . fromStrict
  where
    rmSpaces :: Text -> Text
    rmSpaces = T.foldr h ""
      where
        h ' ' xs | T.null xs || not (isAlphaNum $ T.head xs) = xs
        h x xs = T.cons x xs
