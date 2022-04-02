module Printing (
    texDoc
)   where

import Generator
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Bifunctor ( Bifunctor(second) )

import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as P

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
        h = maybe mempty (text . T.unpack)
        hNest = case (begin, end) of
            (Nothing, Nothing) -> id
            _                  -> nest 2

texDocElement :: Definitions -> Bool -> DocElement -> Doc
texDocElement defs math (DocParagraph els)
        = P.sep $ map (P.hsep . map (texParEl defs math)) els
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
        body  = vcat $ punctuate (text $ T.unpack sep')
                    ((text (T.unpack pref') <>) . texDocImpl defs (math || innerMath) <$> els)
texDocElement _ _ DocEmptyLine = ""

texParEl :: Definitions -> Bool -> ParEl -> Doc
texParEl _    False (ParText    t) = text (T.unpack t)
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Doc
texMath Definitions{mathCmds} = text . T.unpack . foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds
