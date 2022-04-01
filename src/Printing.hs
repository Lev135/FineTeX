module Printing (
    texDoc
)   where

import Generator
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Bifunctor ( Bifunctor(second) )


texDoc :: Definitions -> [DocElement] -> Text
texDoc defs = (<> "\n") . texDocImpl defs False

texDocImpl :: Definitions -> Bool -> [DocElement] -> Text
texDocImpl defs math = T.intercalate "\n" . map (texDocElement defs math)


replaceArgs :: [Argument] -> [ArgV] -> Text -> Text
replaceArgs args argvs = foldr (.) id (zipWith h args argvs)
    where
        h Argument{name} (ArgVString s) = T.replace ("$" <> name) s

surround :: Maybe Text -> Maybe Text -> Bool -> Text -> Text
surround begin end empty txt = b <> txt <> e
    where
        b = fromMaybe T.empty $ if empty then begin else (<> "\n") <$> begin
        e = maybe T.empty ("\n" <> ) end

texDocElement :: Definitions -> Bool -> DocElement -> Text
texDocElement defs math (DocParagraph els)
        = T.intercalate "\n" $ map (mconcat . map (texParEl defs math)) els
texDocElement defs math (DocEnvironment Environment{begin, end, args, innerMath} argvs els)
        = surround (repl <$> begin) (repl <$> end) (null els)
            $ texDocImpl defs (math || innerMath) els
    where
        repl = replaceArgs args argvs
texDocElement defs math (DocPrefGroup Pref{begin, end, pref, sep, innerMath} els)
        = surround begin end (null els) bodyS
    where
        unM = fromMaybe T.empty
        sep'  = fromMaybe T.empty sep <> "\n"
        pref' = maybe T.empty (<> " ") pref
        bodyS = T.intercalate sep' ((pref' <>) . texDocImpl defs (math || innerMath) <$> els)
texDocElement _ _ DocEmptyLine = ""

texParEl :: Definitions -> Bool -> ParEl -> Text
texParEl _    False (ParText    t) = t
texParEl defs False (ParFormula t) = "$" <> texMath defs t <> "$"
texParEl defs True  (ParText    t) = texMath defs t
texParEl defs True  (ParFormula t) = texMath defs t

texMath :: Definitions -> Text -> Text
texMath Definitions{mathCmds} = foldr (.) id fs
    where fs = map (uncurry T.replace . second ((<>" ") . val)) mathCmds
