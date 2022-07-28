module FineTeX.Printer
  ( prettyDoc,
    PrintOpts (..),
    PrefTabMode (..),
    Ann (..),
  )
where

import Data.List.Extra (split)
import Data.Maybe (catMaybes)
import FineTeX.Parser.Syntax (EnvBody (..))
import FineTeX.Processor.Body (Word' (..), WordDocElement (..))
import FineTeX.Utils (Box (..))
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

prettyDoc :: Box p => PrintOpts -> [WordDocElement Word' p] -> Doc
prettyDoc opts = (<> P.line) . prettyEls opts

prettyEls :: Box p => PrintOpts -> [WordDocElement Word' p] -> Doc
prettyEls opts = P.vcat . map (prettyEl opts)

prettyEl :: Box p => PrintOpts -> WordDocElement Word' p -> Doc
prettyEl opts@PrintOpts {tabSize, prefTabMode} = \case
  WDocParagraph ws -> prettyWords ws
  WDocEnvironment _ bws body ews -> P.vcat $ catMaybes [pr bws, pbody, pr ews]
    where
      pr xs = if null xs then Nothing else Just $ prettyWords xs
      pbody = case body of
        VerbBody _ [] -> Nothing
        VerbBody verbInd ls ->
          let ann = if verbInd then P.annotate NoIndent else id
           in Just . ann $ P.vsep (pretty . unBox <$> ls)
        NoVerbBody [] -> Nothing
        NoVerbBody els -> Just $ nest $ prettyEls opts els
      nest
        | null bws && null ews = id
        | otherwise = P.indent tabSize
  WDocPrefItem bws els ews -> case prefTabMode of
    NoTab -> pref' <> els' <> suf'
    NormalTab -> P.hang tabSize $ pref' <> els' <> suf'
    ColumnTab -> pref' <> P.align els' <> suf'
    where
      pref' = prettyWords bws <> if null bws then "" else " "
      suf' = prettyWords ews
      els' = prettyEls opts els
  WDocEmptyLine -> P.emptyDoc

prettyWords :: Box p => [Word' p] -> Doc
prettyWords = P.fillSep . prettyWordsList
  where
    prettyWordsList = map (mconcat . map h) . filter (not . null) . split (== WSpace')
    h = \case
      WString' s -> pretty $ unBox s
      WSpace' -> error "Space"
      WGroup' ws -> P.pageWidth $ \pgWidth ->
        P.nesting $ \nestLvl ->
          if ribbonWidth pgWidth nestLvl > wordsWidth ws
            then P.hsep $ prettyWordsList ws
            else P.fillSep $ prettyWordsList ws
    wordsWidth =
      (\lens -> sum lens + length lens - 1)
        . map wordsWidth
        . filter (not . null)
        . split (== WSpace')

    ribbonWidth (P.AvailablePerLine lineLength ribbonFraction) nestLvl =
      floor $ fromIntegral (lineLength - nestLvl `max` 0) * ribbonFraction
    ribbonWidth P.Unbounded _ = 100000
