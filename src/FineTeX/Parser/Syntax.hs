{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module FineTeX.Parser.Syntax where

import Data.Text (Text)
import FineTeX.Parser.Utils (Posed)
import GHC.Generics (Generic)
import Prelude hiding (Word)

data Document = Document
  { imports :: [Import],
    definitions :: DefBlock,
    body :: [DocElement]
  }
  deriving (Eq, Show, Generic)

-- * Imports

newtype Import = Import
  { filename :: Posed Text
  }
  deriving (Eq, Show, Generic)

type ModeName = Text

-- * Definitions

-- | Block of definitions (@\@Define@)
type DefBlock = [DefSubBlock]

-- | Subblock of definitions
data DefSubBlock
  = -- | Mode block definition (@\@Modes@)
    DefModeBlock [DefMode]
  | -- | Sort block definition (@\@Sorts@)
    DefSortBlock [DefSort]
  | -- | Block of definitions in specific mode (@\@In <modeName>@)
    DefInModeBlock (Posed ModeName) [DefInModeBlock]
  deriving (Eq, Show, Generic)

data DefInModeBlock
  = -- | @\@Environments@
    DefEnvBlock [DefEnvironment]
  | -- | @\@Rules@ (in old versions @\@Commands@)
    DefRuleBlock [DefRule]
  | -- | @\@Prefs@
    DefPrefBlock [DefPref]
  | -- | @\@Inlines@
    DefInlBlock [DefInline]
  deriving (Eq, Show, Generic)

newtype DefMode = DefMode
  {name :: Posed Text}
  deriving (Eq, Show, Generic)

data DefSort = DefSort
  { name :: Posed Text,
    val :: SortExp
  }
  deriving (Eq, Show, Generic)

data DefEnvironment = DefEnvironment
  { name :: Posed Text,
    begin, end :: [RuleTerm],
    args :: [Argument],
    inner :: EnvInner
  }
  deriving (Eq, Show, Generic)

data DefPref = DefPref
  { name :: Posed Text,
    begin, end :: [RuleTerm],
    args :: [Argument],
    pref, suf, sep :: [RuleTerm],
    innerModeName :: Posed Text,
    noPrefInside :: Bool
  }
  deriving (Eq, Show, Generic)

data DefRule = DefRule
  { -- | Left side of rule definition
    match :: PatMatchExp,
    -- | Right side of rule definition
    rule :: [RuleTerm]
  }
  deriving (Eq, Show, Generic)

data DefInline = DefInline
  { borders :: (Posed Text, Posed Text),
    begin, end :: [RuleTerm],
    innerModeName :: Posed Text
  }
  deriving (Eq, Show, Generic)

-- * Parts of definitions

-- ** Environment

-- | Type of environment body. Can be
-- - Normal ('NoVerb'), i. e. environment contains FineTeX code in some mode.
-- - Verbatim ('Verb'). If 'Bool' is True indentation will be also preserved.
data EnvInner = NoVerb EnvNoVerb | Verb Bool
  deriving (Eq, Show, Generic)

data EnvNoVerb = EnvNoVerb
  { innerModeName :: Posed Text,
    noPrefInside :: Bool
  }
  deriving (Eq, Show, Generic)

-- ** Sorts of strings and rules patterns

data PatMatchExp = PatMatchExp
  { behind, ahead :: SortExp,
    current :: [PatMatchEl]
  }
  deriving (Eq, Show, Generic)

-- | Pattern match element
-- @ '(' var ':' sort ')'  |  sort @
data PatMatchEl = PatMatchEl
  { var :: Maybe (Posed VarName),
    sort :: SortExp
  }
  deriving (Eq, Show, Generic)

type VarName = Text

type SortName = Text

-- |  Sort of string. Something like RegExps without stars (at the moment)
data SortExp
  = -- | Match only this string
    SEString (Posed Text)
  | -- | Match some spaces/tabs/eols
    SESpace
  | -- | Match a sort with this name
    SESort (Posed SortName)
  | -- | Match nothing (always fails). Identity of 'SEOr'
    SEVoid
  | -- | Match empty string. Identity of 'SEConcat'
    SEEmpty
  | -- | Match two sorts consequently
    SEConcat SortExp SortExp
  | -- | Match one sort or another
    SEOr SortExp SortExp
  deriving (Eq, Show, Generic)

-- | Term of the right part of rules, sections of prefs and envs
data RuleTerm
  = -- | String constant
    RTString (Posed Text)
  | -- | Matched variable (identifier)
    RTVar (Posed VarName)
  | -- | Space/eol symbol
    RTSpace
  | -- | Rule terms need to be processed in particular mode
    -- (if Nothing, current mode is used)
    RTRun (Maybe (Posed ModeName)) [RuleTerm]
  deriving (Eq, Show, Generic)

-- ** Arguments

data Argument = Argument
  { name :: Posed Text,
    kind :: ArgKind
  }
  deriving (Eq, Show, Generic)

data ArgKind = AKString | AKSort SortExp
  deriving (Eq, Show, Generic)

-- * Document body

data DocElement
  = DocParLine [ParEl]
  | DocEnvironment (Posed Text) [ArgVal] (EnvBody DocElement)
  | DocPref (Posed Text) [ArgVal] [DocElement]
  | DocEmptyLine
  | DocCommentLine (Posed Text)
  deriving (Eq, Show, Generic)

data ParEl
  = ParText [WordOrSpace]
  | ParInline Text [ParEl]
  deriving (Eq, Show, Generic)

data EnvBody el
  = VerbBody Bool [Posed Text]
  | NoVerbBody [el]
  deriving (Eq, Show, Generic)

mapEnvBody :: ([el] -> [el']) -> EnvBody el -> EnvBody el'
mapEnvBody _ (VerbBody b els) = VerbBody b els
mapEnvBody f (NoVerbBody els) = NoVerbBody $ f els

mapMEnvBody :: Applicative m => ([el] -> m [el']) -> EnvBody el -> m (EnvBody el')
mapMEnvBody _ (VerbBody b els) = pure $ VerbBody b els
mapMEnvBody f (NoVerbBody els) = NoVerbBody <$> f els

data Word
  = -- | String, that will be printed in output as it is, without any processing
    WString (Posed Text)
  | -- | Word to be tokenized. Tokens will be replaced by command rules
    WWord (Posed Text)
  | -- | Space will be printed as space or eol
    WSpace
  | -- | Group of words in some mode
    WMode ModeName [Word]
  | -- | Group of words. Printer will try not to split it between lines
    WGroup [Word]
  deriving (Eq, Show, Generic)

data WordOrSpace = ParWord (Posed Text) | ParSpace
  deriving (Eq, Show, Generic)

data ArgVal = AVString (Posed Text) | AVSort (Posed Text)
  deriving (Eq, Show, Generic)
