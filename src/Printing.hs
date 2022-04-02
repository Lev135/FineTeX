module Printing (
    texDoc
)   where

import Generator
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Bifunctor ( Bifunctor(second) )

import Text.PrettyPrint ( ($+$), Doc )
import qualified Text.PrettyPrint as P

text :: Text -> Doc
text = P.text . T.unpack

texDoc :: Definitions -> [DocElement] -> Doc
texDoc defs = (<> "\n") . texDocImpl defs False

texDocImpl :: Definitions -> Bool -> [DocElement] -> Doc
texDocImpl defs math = P.vcat . map (texDocElement defs math)


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: Maybe Text -> Maybe Text -> Bool -> Doc -> Doc
surround begin end empty txt = h begin $+$ hNest txt $+$ h end
    where
        h = maybe mempty text
        hNest = case (begin, end) of
            (Nothing, Nothing) -> id
            _                  -> P.nest 2

texDocElement :: Definitions -> Bool -> DocElement -> Doc
texDocElement defs math (DocParagraph els)
        = P.fsep $ map (P.fsep . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = surround (repl <$> begin) (repl <$> end) (null els)
            $ texDocImpl defs (math || innerMath) els
    where
        repl = replaceArgs args argvs
texDocElement defs math (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround begin end (null els) body
    where
        sep'  = fromMaybe T.empty sep
        pref' = maybe T.empty (<> " ") pref
        body  = P.vcat $ P.punctuate (text sep')
                    ((text pref' <>) . texDocImpl defs (math || innerMath) <$> els)
texDocElement _ _ DocEmptyLine = ""

texParEl :: Definitions -> Bool -> ParEl -> Doc
texParEl _    False (ParText    t) = P.fsep $ text <$> t
texParEl defs False (ParFormula t) = "$" <> P.fsep (texMath defs <$> t) <> "$"
texParEl defs True  (ParText    t) = P.fsep $ texMath defs <$> t
texParEl defs True  (ParFormula t) = P.fsep  $ texMath defs <$> t

texMath :: Definitions -> Text -> Doc
texMath Definitions{mathCmds} = text . foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds

