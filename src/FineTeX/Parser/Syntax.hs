{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Parser.Syntax where

import Control.Lens (makeFieldsNoPrefix, makePrisms)
import Data.Text (Text)
import FineTeX.Parser.Utils (Posed)
import Prelude hiding (Word)

data Document = Document
  { _imports :: [Import],
    _definitions :: DefBlock,
    _body :: [DocElement]
  }
  deriving (Eq, Show)

-- * Imports

newtype Import = Import
  { _filename :: Posed Text
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data DefInModeBlock
  = -- | @\@Environments@
    DefEnvBlock [DefEnvironment]
  | -- | @\@Rules@ (in old versions @\@Commands@)
    DefRuleBlock [DefRule]
  | -- | @\@Prefs@
    DefPrefBlock [DefPref]
  | -- | @\@Inlines@
    DefInlBlock [DefInline]
  deriving (Eq, Show)

newtype DefMode = DefMode
  {_name :: Posed Text}
  deriving (Eq, Show)

data DefSort = DefSort
  { _name :: Posed Text,
    _val :: SortExp
  }
  deriving (Eq, Show)

data DefEnvironment = DefEnvironment
  { _name :: Posed Text,
    _begin, _end :: [RuleTerm],
    _args :: [Argument],
    _inner :: EnvInner
  }
  deriving (Eq, Show)

data DefPref = DefPref
  { _name :: Posed Text,
    _begin, _end :: [RuleTerm],
    _args :: [Argument],
    _pref, _suf, _sep :: [RuleTerm],
    _innerModeName :: Posed Text,
    _noPrefInside :: Bool
  }
  deriving (Eq, Show)

data DefRule = DefRule
  { -- | Left side of rule definition
    _match :: PatMatchExp,
    -- | Right side of rule definition
    _rule :: [RuleTerm]
  }
  deriving (Eq, Show)

data DefInline = DefInline
  { _borders :: (Posed Text, Posed Text),
    _begin, _end :: [RuleTerm],
    _innerModeName :: Posed Text
  }
  deriving (Eq, Show)

-- * Parts of definitions

-- ** Environment

-- | Type of environment body. Can be
-- - Normal ('NoVerb'), i. e. environment contains FineTeX code in some mode.
-- - Verbatim ('Verb'). If 'Bool' is True indentation will be also preserved.
data EnvInner = NoVerb EnvNoVerb | Verb Bool
  deriving (Eq, Show)

data EnvNoVerb = EnvNoVerb
  { _innerModeName :: Posed Text,
    _noPrefInside :: Bool
  }
  deriving (Eq, Show)

-- ** Sorts of strings and rules patterns

data PatMatchExp = PatMatchExp
  { _behind, _ahead :: SortExp,
    _current :: [PatMatchEl]
  }
  deriving (Eq, Show)

-- | Pattern match element
-- @ '(' var ':' sort ')'  |  sort @
data PatMatchEl = PatMatchEl
  { _var :: Maybe (Posed VarName),
    _sort :: SortExp
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- ** Arguments

data Argument = Argument
  { _name :: Posed Text,
    _kind :: ArgKind
  }
  deriving (Eq, Show)

data ArgKind = AKString | AKSort SortExp
  deriving (Eq, Show)

-- * Document body

data DocElement
  = DocParLine [ParEl]
  | DocEnvironment (Posed Text) [ArgVal] (EnvBody DocElement)
  | DocPref (Posed Text) [ArgVal] [DocElement]
  | DocEmptyLine
  | DocCommentLine (Posed Text)
  deriving (Eq, Show)

data ParEl
  = ParText [WordOrSpace]
  | ParInline Text [ParEl]
  deriving (Eq, Show)

data EnvBody el
  = VerbBody Bool [Posed Text]
  | NoVerbBody [el]
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data WordOrSpace = ParWord (Posed Text) | ParSpace
  deriving (Eq, Show)

data ArgVal = AVString (Posed Text) | AVSort (Posed Text)
  deriving (Eq, Show)

makeFieldsNoPrefix ''Document
makeFieldsNoPrefix ''Import
makeFieldsNoPrefix ''DefMode
makeFieldsNoPrefix ''DefSort
makeFieldsNoPrefix ''DefEnvironment
makeFieldsNoPrefix ''DefPref
makeFieldsNoPrefix ''DefRule
makeFieldsNoPrefix ''DefInline
makeFieldsNoPrefix ''Argument
makeFieldsNoPrefix ''EnvNoVerb
makePrisms ''EnvInner
