module Printing (
    texDoc
)   where

import Generator
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bifunctor ( Bifunctor(second) )

import Prettyprinter (pretty)
import qualified Prettyprinter as P
import Data.Void (Void)

type Doc = P.Doc Void

texDoc :: Definitions -> [DocElement] -> Doc 
texDoc defs = (<> "\n") . texDocImpl defs False

texDocImpl :: Definitions -> Bool -> [DocElement] -> Doc
texDocImpl defs math = P.vcat . map (texDocElement defs math)


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: P.Pretty a => Maybe a -> Maybe a -> Doc -> Doc
surround begin end txt = P.vsep $ catMaybes [pretty <$> begin, Just $ hNest txt, pretty <$> end]
    where
        hNest = case (begin, end) of
            (Nothing, Nothing) -> id
            _                  -> P.indent 2

texDocElement :: Definitions -> Bool -> DocElement -> Doc
texDocElement defs math (DocParagraph els)
        = P.align $ P.fillSep $ map (P.fillSep . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = surround (repl <$> begin) (repl <$> end)
            $ texDocImpl defs (math || innerMath) els
    where
        repl = replaceArgs args argvs
texDocElement defs math (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround begin end body
    where
        sep'  = fromMaybe T.empty sep
        pref' = maybe T.empty (<> " ") pref
        body  = P.vcat $ P.punctuate (pretty sep')
                    ((pretty pref' <>) . texDocImpl defs (math || innerMath) <$> els)
texDocElement _ _ DocEmptyLine = ""

texParEl :: Definitions -> Bool -> ParEl -> Doc
texParEl _    False (ParText    t) = P.fillSep $ pretty <$> t
texParEl defs False (ParFormula t) = "$" <> P.fillSep (texMath defs <$> t) <> "$"
texParEl defs True  (ParText    t) = P.fillSep $ texMath defs <$> t
texParEl defs True  (ParFormula t) = P.fillSep  $ texMath defs <$> t

texMath :: Definitions -> Text -> Doc
texMath Definitions{mathCmds} = pretty . foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds

