module Printer
  ( texDoc,
    PrintOpts (..),
    PrefTabMode (..),
    Ann (..),
  )
where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Parser
  ( ArgV (..),
    Argument (Argument, name),
    DocElement (..),
    Environment (..),
    ParEl (..),
    Pref (..),
    VerbMode (NoVerb, VerbIndent),
    oneLine,
  )
import Prettyprinter (pretty)
import qualified Prettyprinter as P

data Ann = NoIndent
  deriving (Show)

type Doc = P.Doc Ann

data PrefTabMode
  = NoTab
  | NormalTab
  | ColumnTab

data PrintOpts = PrintOpts
  { tabSize :: Int,
    prefTabMode :: PrefTabMode
  }

texDoc :: PrintOpts -> [DocElement] -> Doc
texDoc opts = (<> "\n") . texDocImpl opts

texDocImpl :: PrintOpts -> [DocElement] -> Doc
texDocImpl opts = P.vcat . map (texDocElement opts)

replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
  where
    h Argument {name} (ArgVString s) = T.replace ("$" <> name) s

surround :: P.Pretty a => PrintOpts -> VerbMode -> Bool -> Maybe a -> Maybe a -> Doc -> Doc
surround PrintOpts {tabSize} verbMode oneLine begin end body =
  annotate $
    sep $ catMaybes [pretty <$> begin, Just $ hNest body, pretty <$> end]
  where
    isVerb = case verbMode of
      VerbIndent -> True
      _ -> False
    annotate
      | isVerb = P.annotate NoIndent
      | otherwise = id
    sep
      | oneLine = P.fillSep
      | otherwise = P.vsep
    hNest
      | isVerb || oneLine = id
      | otherwise = case (begin, end) of
        (Nothing, Nothing) -> id
        _ -> P.indent tabSize

texDocElement :: PrintOpts -> DocElement -> Doc
texDocElement _ (DocParagraph els) =
  P.fillSep $ map (P.hcat . map texParEl) els
texDocElement opts (DocEnvironment Environment {begin, end, args, innerVerb} argvs els) =
  surround opts innerVerb False (repl <$> begin) (repl <$> end) $
    texDocImpl opts els
  where
    repl = replaceArgs args argvs
texDocElement opts@PrintOpts {prefTabMode, tabSize} (DocPrefGroup Pref {begin, end, args, pref, sep, oneLine} els) =
  surround opts NoVerb oneLine begin end body
  where
    sep' = fromMaybe T.empty sep
    body = cat $ P.punctuate (pretty sep') (prettyEl <$> els)
    cat
      | oneLine = P.fillSep
      | otherwise = P.vcat
    prettyEl :: ([ArgV], [DocElement]) -> Doc
    prettyEl (argvs, els) = case prefTabMode of
      NoTab -> pref'' <> els'
      NormalTab -> P.hang tabSize $ pref'' <> els'
      ColumnTab -> pref'' <> P.align els'
      where
        pref'' = pretty $ replaceArgs args argvs $ maybe T.empty (<> " ") pref
        els' = texDocImpl opts els
texDocElement _ DocEmptyLine = ""
texDocElement _ (DocVerb _ txts) = P.vsep $ pretty <$> txts

texParEl :: ParEl -> Doc
texParEl (ParText t) = P.fillSep $ pretty . fst <$> t
texParEl (ParFormula t) = P.pageWidth $ \pgWidth ->
  P.nesting $ \nestLvl ->
    if ribbonWidth pgWidth nestLvl > elsWidth
      then sepLayout
      else fillLayout
  where
    sepLayout = P.enclose "$" "$" $ P.hsep $ pretty . fst <$> t
    fillLayout = P.enclose "$" "$" $ P.fillSep $ pretty . fst <$> t
    elsWidth = sum (T.length . fst <$> t) + (length t + 1)
    ribbonWidth (P.AvailablePerLine lineLength ribbonFraction) nestLvl =
      floor $ fromIntegral (lineLength - nestLvl `max` 0) * ribbonFraction
    ribbonWidth P.Unbounded _ = 100000
