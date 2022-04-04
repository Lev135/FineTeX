module Printing (
    texDoc,
    PrintOpts (..), PrefTabMode (..)
)   where

import Generator
    ( ParEl(..),
      DocElement(..),
      Definitions(Definitions, mathCmds),
      Command(val),
      Pref(Pref, innerMath, sep, pref, end, begin),
      Environment(Environment, innerMath, args, end, begin),
      ArgV(..),
      Argument(Argument, name) )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bifunctor ( Bifunctor(second) )

import Data.Map (Map)
import qualified Data.Map as M
import Text.Replace ( Trie, replaceWithTrie, mapToTrie, text'fromText )

import Prettyprinter (pretty)
import qualified Prettyprinter as P
import Data.Void (Void)
import Data.List (intersperse)
import Data.Char (isAlphaNum)
import Data.Text.Lazy (fromStrict, toStrict)
type Doc = P.Doc Void

newtype Tries = Tries {
        mathCmdsTrie :: Trie
    }

data PrefTabMode
    = NoTab
    | NormalTab
    | ColumnTab

data PrintOpts = PrintOpts {
        tabSize         :: Int,
        prefTabMode     :: PrefTabMode
    }

texDoc :: PrintOpts -> Definitions -> [DocElement] -> Doc
texDoc opts Definitions{mathCmds} = (<> "\n") . texDocImpl opts tries False
    where tries = Tries {
            mathCmdsTrie = mapToTrie . M.map (( <> " ") . val) . M.mapKeys text'fromText $  mathCmds
        }

texDocImpl :: PrintOpts -> Tries -> Bool -> [DocElement] -> Doc
texDocImpl opts defs math = P.vcat . map (texDocElement opts defs math)


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: P.Pretty a => PrintOpts -> Maybe a -> Maybe a -> Doc -> Doc
surround PrintOpts{tabSize} begin end txt = P.vsep $ catMaybes [pretty <$> begin, Just $ hNest txt, pretty <$> end]
    where
        hNest = case (begin, end) of
            (Nothing, Nothing) -> id
            _                  -> P.indent tabSize

texDocElement :: PrintOpts -> Tries -> Bool -> DocElement -> Doc
texDocElement _ defs math (DocParagraph els)
        = P.fillSep $ map (P.hcat . map (texParEl defs math)) els
texDocElement opts defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = surround opts (repl <$> begin) (repl <$> end)
            $ texDocImpl opts defs (math || innerMath) els
    where
        repl = replaceArgs args argvs
texDocElement opts@PrintOpts{prefTabMode, tabSize} defs math (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround opts begin end body
    where
        sep'  = fromMaybe T.empty sep
        pref' = maybe T.empty (<> " ") pref
        body  = P.vcat $ P.punctuate (pretty sep')
                    (preftab . texDocImpl opts defs (math || innerMath) <$> els)
        preftab = case prefTabMode of
            NoTab     -> (pretty pref' <>)
            NormalTab -> P.hang tabSize . (pretty pref' <>)
            ColumnTab -> (pretty pref' <>) . P.align
texDocElement _ _ _ DocEmptyLine = ""
texDocElement _ _ _ (DocVerb txts) = P.vsep $ pretty <$> txts
        
texParEl :: Tries -> Bool -> ParEl -> Doc
texParEl _    False (ParText    t) = P.fillSep $ pretty <$> t
texParEl defs False (ParFormula t) = P.pageWidth $ \pgWidth ->
    P.nesting $ \nestLvl -> 
        if ribbonWidth pgWidth nestLvl > elsWidth
            then sepLayout
            else fillLayout
    where
        sepLayout   = P.enclose "$" "$" $ P.hsep    $ pretty <$> els
        fillLayout  = P.enclose "$" "$" $ P.fillSep $ pretty <$> els
        elsWidth    = sum (T.length <$> els) + (length els + 1) 
        ribbonWidth (P.AvailablePerLine lineLength ribbonFraction) nestLvl 
            = floor $ fromIntegral (lineLength - nestLvl `max` 0) * ribbonFraction
        ribbonWidth P.Unbounded _ = 100000
        els = texMath defs <$> t

texParEl defs True  (ParText    t) = P.fillSep $ pretty . texMath defs <$> t
texParEl defs True  (ParFormula t) = P.fillSep $ pretty . texMath defs <$> t

texMath :: Tries -> Text -> Text
texMath Tries{mathCmdsTrie} = rmSpaces . toStrict . replaceWithTrie mathCmdsTrie . fromStrict
    where
        rmSpaces :: Text -> Text
        rmSpaces = T.foldr h ""
            where
                h ' ' xs | T.null xs || not (isAlphaNum $ T.head xs) = xs
                h x xs = T.cons x xs
