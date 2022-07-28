{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Processor.Syntax where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Text (Text)
import FineTeX.Parser.Syntax
import FineTeX.Utils (Box, Wrap)
import Prelude hiding (Word)

data Definitions p = Definitions
  { _modes :: Map Text (DefMode p),
    _inModes :: Map Text (Wrap p InModeDefs)
  }

data InModeDefs p = InModeDefs
  { _envs :: Map Text (DefEnvironment p),
    _cmds :: Map Text (DefCommand p),
    _prefs :: Map Text (DefPref p),
    _inlines :: Map Text (DefInline p)
  }

deriving instance Box p => Show (Definitions p)

deriving instance Box p => Show (InModeDefs p)

deriving instance Box p => Eq (Definitions p)

deriving instance Box p => Eq (InModeDefs p)

instance Box p => Semigroup (Definitions p) where
  Definitions a b <> Definitions a' b' = Definitions (a <> a') (b <> b')

instance Box p => Monoid (Definitions p) where
  mempty = Definitions mempty mempty

instance Box p => Semigroup (InModeDefs p) where
  InModeDefs a b c d <> InModeDefs a' b' c' d' =
    InModeDefs (a <> a') (b <> b') (c <> c') (d <> d')

instance Box p => Monoid (InModeDefs p) where
  mempty = InModeDefs mempty mempty mempty mempty

makeLenses ''Definitions
makeLenses ''InModeDefs
