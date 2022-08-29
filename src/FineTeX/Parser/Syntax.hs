{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Parser.Syntax where

import Control.Lens (makeFieldsNoPrefix, makePrisms)
import Data.Text (Text)
import FineTeX.Utils (Box)
import Prelude hiding (Word)

data Document p = Document
  { _imports :: [Import p],
    _definitions :: DefBlock p,
    _body :: [DocElement p]
  }
  deriving (Eq, Show)

-- * Imports

newtype Import p = Import
  { _filename :: p Text
  }

type ModeName = Text

-- * Definitions

-- | Block of definitions (@\@Define@)
type DefBlock p = [DefSubBlock p]

-- | Subblock of definitions
data DefSubBlock p
  = -- | Mode block definition (@\@Modes@)
    DefModeBlock [DefMode p]
  | -- | Sort block definition (@\@Sorts@)
    DefSortBlock [DefSort p]
  | -- | Block of definitions in specific mode (@\@In <modeName>@)
    DefInModeBlock (p ModeName) [DefInModeBlock p]

data DefInModeBlock p
  = -- | @\@Environments@
    DefEnvBlock [DefEnvironment p]
  | -- | @\@Rules@ (in old versions @\@Commands@)
    DefRuleBlock [DefRule p]
  | -- | @\@Prefs@
    DefPrefBlock [DefPref p]
  | -- | @\@Inlines@
    DefInlBlock [DefInline p]

newtype DefMode p = DefMode
  {_name :: p Text}

data DefSort p = DefSort
  { _name :: p Text,
    _val :: SortExp p
  }

data DefEnvironment p = DefEnvironment
  { _name :: p Text,
    _begin, _end :: [RuleTerm p],
    _args :: [Argument p],
    _inner :: EnvInner p
  }

data DefPref p = DefPref
  { _name :: p Text,
    _begin, _end :: [RuleTerm p],
    _args :: [Argument p],
    _pref, _suf, _sep :: [RuleTerm p],
    _innerModeName :: p Text,
    _noPrefInside :: Bool
  }

data DefRule p = DefRule
  { -- | Left side of rule definition
    _match :: PatMatchExp p,
    -- | Right side of rule definition
    _rule :: [RuleTerm p]
  }

data DefInline p = DefInline
  { _borders :: (p Text, p Text),
    _begin, _end :: [RuleTerm p],
    _innerModeName :: p Text
  }

-- * Parts of definitions

-- ** Environment

-- | Type of environment body. Can be
-- - Normal ('NoVerb'), i. e. environment contains FineTeX code in some mode.
-- - Verbatim ('Verb'). If 'Bool' is True indentation will be also preserved.
data EnvInner p = NoVerb (EnvNoVerb p) | Verb Bool

data EnvNoVerb p = EnvNoVerb
  { _innerModeName :: p Text,
    _noPrefInside :: Bool
  }

-- ** Sorts of strings and rules patterns

data PatMatchExp p = PatMatchExp
  { _behind, _ahead :: SortExp p,
    _current :: [PatMatchEl p]
  }

-- | Pattern match element
-- @ '(' var ':' sort ')'  |  sort @
data PatMatchEl p = PatMatchEl
  { _var :: Maybe (p VarName),
    _sort :: SortExp p
  }

type VarName = Text

type SortName = Text

-- |  Sort of string. Something like RegExps without stars (at the moment)
data SortExp p
  = -- | Match only this string
    SEString (p Text)
  | -- | Match some spaces/tabs/eols
    SESpace
  | -- | Match a sort with this name
    SESort (p SortName)
  | -- | Match nothing (always fails). Identity of 'SEOr'
    SEVoid
  | -- | Match empty string. Identity of 'SEConcat'
    SEEmpty
  | -- | Match two sorts consequently
    SEConcat (SortExp p) (SortExp p)
  | -- | Match one sort or another
    SEOr (SortExp p) (SortExp p)

-- | Term of the right part of rules, sections of prefs and envs
data RuleTerm p
  = -- | String constant
    RTString (p Text)
  | -- | Matched variable (identifier)
    RTVar (p VarName)
  | -- | Space/eol symbol
    RTSpace
  | -- | Rule terms need to be processed in particular mode
    -- (if Nothing, current mode is used)
    RTRun (Maybe (p ModeName)) [RuleTerm p]

-- ** Arguments

data Argument p = Argument
  { _name :: p Text,
    _kind :: ArgKind p
  }

data ArgKind p = AKString | AKSort (SortExp p)
  deriving (Eq, Show)

-- * Document body

data DocElement p
  = DocParLine [ParEl p]
  | DocEnvironment (p Text) [ArgVal p] (EnvBody DocElement p)
  | DocPref (p Text) [ArgVal p] [DocElement p]
  | DocEmptyLine
  | DocCommentLine (p Text)

data ParEl p
  = ParText [WordOrSpace p]
  | ParInline Text [ParEl p]

data EnvBody el p
  = VerbBody Bool [p Text]
  | NoVerbBody [el p]

mapEnvBody :: ([el p] -> [el' p]) -> EnvBody el p -> EnvBody el' p
mapEnvBody _ (VerbBody b els) = VerbBody b els
mapEnvBody f (NoVerbBody els) = NoVerbBody $ f els

mapMEnvBody :: Applicative m => ([el p] -> m [el' p]) -> EnvBody el p -> m (EnvBody el' p)
mapMEnvBody _ (VerbBody b els) = pure $ VerbBody b els
mapMEnvBody f (NoVerbBody els) = NoVerbBody <$> f els

data Word p
  = -- | String, that will be printed in output as it is, without any processing
    WString (p Text)
  | -- | Word to be tokenized. Tokens will be replaced by command rules
    WWord (p Text)
  | -- | Space will be printed as space or eol
    WSpace
  | -- | Group of words in some mode
    WMode ModeName [Word p]
  | -- | Group of words. Printer will try not to split it between lines
    WGroup [Word p]

data WordOrSpace p = ParWord (p Text) | ParSpace

data ArgVal p = AVString (p Text) | AVSort (p Text)

deriving instance Box p => Show (Import p)

deriving instance Box p => Show (DefSubBlock p)

deriving instance Box p => Show (DefInModeBlock p)

deriving instance Box p => Show (DefMode p)

deriving instance Box p => Show (DefEnvironment p)

deriving instance Box p => Show (DefPref p)

deriving instance Box p => Show (DefInline p)

deriving instance Box p => Show (Word p)

deriving instance Box p => Show (EnvInner p)

deriving instance Box p => Show (EnvNoVerb p)

deriving instance Box p => Show (Argument p)

deriving instance Box p => Show (DocElement p)

deriving instance Box p => Show (ParEl p)

deriving instance (Show (el p), Box p) => Show (EnvBody el p)

deriving instance Box p => Show (WordOrSpace p)

deriving instance Box p => Show (ArgVal p)

deriving instance Box p => Show (DefSort p)

deriving instance Box p => Show (DefRule p)

deriving instance Box p => Show (RuleTerm p)

deriving instance Box p => Show (PatMatchExp p)

deriving instance Box p => Show (PatMatchEl p)

deriving instance Box p => Show (SortExp p)

deriving instance Box p => Eq (Import p)

deriving instance Box p => Eq (DefSubBlock p)

deriving instance Box p => Eq (DefInModeBlock p)

deriving instance Box p => Eq (DefMode p)

deriving instance Box p => Eq (DefEnvironment p)

deriving instance Box p => Eq (EnvInner p)

deriving instance Box p => Eq (EnvNoVerb p)

deriving instance Box p => Eq (DefPref p)

deriving instance Box p => Eq (DefRule p)

deriving instance Box p => Eq (DefInline p)

deriving instance Box p => Eq (Word p)

deriving instance Box p => Eq (Argument p)

deriving instance Box p => Eq (DocElement p)

deriving instance Box p => Eq (ParEl p)

deriving instance (Eq (el p), Box p) => Eq (EnvBody el p)

deriving instance Box p => Eq (WordOrSpace p)

deriving instance Box p => Eq (ArgVal p)

deriving instance Box p => Eq (DefSort p)

deriving instance Box p => Eq (RuleTerm p)

deriving instance Box p => Eq (PatMatchExp p)

deriving instance Box p => Eq (PatMatchEl p)

deriving instance Box p => Eq (SortExp p)

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
