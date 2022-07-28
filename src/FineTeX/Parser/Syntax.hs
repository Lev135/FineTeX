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

-- Imports

newtype Import p = Import
  { _filename :: p Text
  }

type ModeName = Text

-- Definitions

type DefBlock p = [DefSubBlock p]

data DefSubBlock p
  = DefModeBlock [DefMode p]
  | DefInModeBlock (p ModeName) [DefInModeBlock p]

-- DefTypeBlock [DefType p]

data DefInModeBlock p
  = DefEnvBlock [DefEnvironment p]
  | DefCmdBlock [DefCommand p]
  | DefPrefBlock [DefPref p]
  | DefInlBlock [DefInline p]

newtype DefMode p = DefMode
  {_name :: p Text}

-- data DefType p
--   deriving (Show)

data DefEnvironment p = DefEnvironment
  { _name :: p Text,
    _begin, _end :: [Word p],
    _args :: [Argument p],
    _inner :: EnvInner p
  }

data DefPref p = DefPref
  { _name :: p Text,
    _begin, _end :: [Word p],
    _args :: [Argument p],
    _pref, _suf, _sep :: [Word p],
    _innerModeName :: p Text,
    _noPrefInside :: Bool
  }

data DefCommand p = DefCommand
  { _name :: p Text,
    _val :: [Word p]
  }

data DefInline p = DefInline
  { _borders :: (p Text, p Text),
    _begin, _end :: [Word p],
    _innerModeName :: p Text
  }

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

-- | Type of environment body. Can be
-- - Normal ('NoVerb'), i. e. environment contains FineTeX code in some mode.
-- - Verbatim ('Verb'). If 'Bool' is True indentation will be also preserved.
data EnvInner p = NoVerb (EnvNoVerb p) | Verb Bool

data EnvNoVerb p = EnvNoVerb
  { _innerModeName :: p Text,
    _noPrefInside :: Bool
  }

data Argument p = Argument
  { _kind :: p ArgKind,
    _name :: p Text
  }

data ArgKind = AKString | AKWord -- AKType Text
  deriving (Eq, Ord, Show)

-- Body

data DocElement p
  = DocParagraph [[ParEl p]]
  | DocEnvironment (p Text) [ArgVal p] (EnvBody DocElement p)
  | DocPref (p Text) [ArgVal p] [DocElement p]
  | DocEmptyLine

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

data WordOrSpace p = ParWord (p Text) | ParSpace

data ArgVal p = AVString (p Text) | AVWord (p Text) -- AVType

deriving instance Box p => Show (Import p)

deriving instance Box p => Show (DefSubBlock p)

deriving instance Box p => Show (DefInModeBlock p)

deriving instance Box p => Show (DefMode p)

deriving instance Box p => Show (DefEnvironment p)

deriving instance Box p => Show (DefPref p)

deriving instance Box p => Show (DefCommand p)

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

deriving instance Box p => Eq (Import p)

deriving instance Box p => Eq (DefSubBlock p)

deriving instance Box p => Eq (DefInModeBlock p)

deriving instance Box p => Eq (DefMode p)

deriving instance Box p => Eq (DefEnvironment p)

deriving instance Box p => Eq (EnvInner p)

deriving instance Box p => Eq (EnvNoVerb p)

deriving instance Box p => Eq (DefPref p)

deriving instance Box p => Eq (DefCommand p)

deriving instance Box p => Eq (DefInline p)

deriving instance Box p => Eq (Word p)

deriving instance Box p => Eq (Argument p)

deriving instance Box p => Eq (DocElement p)

deriving instance Box p => Eq (ParEl p)

deriving instance (Eq (el p), Box p) => Eq (EnvBody el p)

deriving instance Box p => Eq (WordOrSpace p)

deriving instance Box p => Eq (ArgVal p)

makeFieldsNoPrefix ''Document
makeFieldsNoPrefix ''Import
makeFieldsNoPrefix ''DefMode
makeFieldsNoPrefix ''DefEnvironment
makeFieldsNoPrefix ''DefPref
makeFieldsNoPrefix ''DefCommand
makeFieldsNoPrefix ''DefInline
makeFieldsNoPrefix ''Argument
makeFieldsNoPrefix ''EnvNoVerb
makePrisms ''EnvInner
