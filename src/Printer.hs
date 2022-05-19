module Printer (
    texDoc,
    PrintOpts (..), PrefTabMode (..),
    Ann (..)
)   where

import Generator
    ( ParEl(..),
      DocElement(..),
      Definitions(Definitions, mathCmds),
      Command(val),
      Pref(Pref, innerMath, sep, pref, end, begin),
      Environment(..),
      ArgV(..),
      Argument(Argument, name), VerbMode (NoVerb, VerbIndent), Pos )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bifunctor ( Bifunctor(second) )

import Data.Map (Map)
import qualified Data.Map as M

import Prettyprinter (pretty)
import qualified Prettyprinter as P
import Data.Void (Void)
import Data.List (intersperse)
import Data.Char (isAlphaNum, isAscii)
import Data.Text.Lazy (fromStrict, toStrict)

data Ann = NoIndent
    deriving Show

type Doc = P.Doc Ann

data PrefTabMode
    = NoTab
    | NormalTab
    | ColumnTab

data PrintOpts = PrintOpts {
        tabSize         :: Int,
        prefTabMode     :: PrefTabMode
    }

texDoc :: PrintOpts -> [DocElement] -> Doc
texDoc opts = (<> "\n") . texDocImpl opts

texDocImpl :: PrintOpts  -> [DocElement] -> Doc
texDocImpl opts = P.vcat . map (texDocElement opts)

replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: P.Pretty a => PrintOpts -> VerbMode -> Maybe a -> Maybe a -> Doc -> Doc
surround _ VerbIndent begin end txt = P.annotate NoIndent
                    $ P.vsep $ catMaybes [pretty <$> begin, Just txt, pretty <$> end]
surround PrintOpts{tabSize} _ begin end txt =  P.vsep $ catMaybes [pretty <$> begin, Just $ hNest txt, pretty <$> end]
    where
        hNest = case (begin, end) of
            (Nothing, Nothing) -> id
            _                  -> P.indent tabSize

texDocElement :: PrintOpts -> DocElement -> Doc
texDocElement _ (DocParagraph els)
        = P.fillSep $ map (P.hcat . map texParEl) els
texDocElement opts (DocEnvironment Environment{begin, end, args, innerMath, innerVerb} argvs els)
        = surround opts innerVerb (repl <$> begin) (repl <$> end)
            $ texDocImpl opts els
    where
        repl = replaceArgs args argvs
texDocElement opts@PrintOpts{prefTabMode, tabSize} (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround opts NoVerb begin end body
    where
        sep'  = fromMaybe T.empty sep
        pref' = maybe T.empty (<> " ") pref
        body  = P.vcat $ P.punctuate (pretty sep')
                    (preftab . texDocImpl opts <$> els)
        preftab = case prefTabMode of
            NoTab     -> (pretty pref' <>)
            NormalTab -> P.hang tabSize . (pretty pref' <>)
            ColumnTab -> (pretty pref' <>) . P.align
texDocElement _ DocEmptyLine = ""
texDocElement _ (DocVerb noInd txts) = P.vsep $ pretty <$> txts

texParEl :: ParEl -> Doc
texParEl (ParText    t) = P.fillSep $ pretty . fst  <$> t
texParEl (ParFormula t) = P.pageWidth $ \pgWidth ->
    P.nesting $ \nestLvl ->
        if ribbonWidth pgWidth nestLvl > elsWidth
            then sepLayout
            else fillLayout
    where
        sepLayout   = P.enclose "$" "$" $ P.hsep    $ pretty . fst <$> t
        fillLayout  = P.enclose "$" "$" $ P.fillSep $ pretty . fst <$> t
        elsWidth    = sum (T.length . fst <$> t) + (length t + 1)
        ribbonWidth (P.AvailablePerLine lineLength ribbonFraction) nestLvl
            = floor $ fromIntegral (lineLength - nestLvl `max` 0) * ribbonFraction
        ribbonWidth P.Unbounded _ = 100000
