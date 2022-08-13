{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Processor.Syntax where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Text (Text)
import FineTeX.Parser.Syntax
import FineTeX.Processor.Tokenizer (BlackWhiteSetList)
import qualified FineTeX.Processor.Tokenizer as Tok
import FineTeX.Utils (Box, Wrap)
import Prelude hiding (Word)

data Definitions p = Definitions
  { _modes :: Map Text (DefMode p),
    -- | Sorts tokens by name
    _sorts :: Map Text (p [BlackWhiteSetList Char]),
    _inModes :: Map Text (Wrap p InModeDefs)
  }

type Id = Int

data InModeDefs p = InModeDefs
  { -- | Environments by name
    _envs :: Map Text (DefEnvironment p),
    -- | Prefs by name (pref string)
    _prefs :: Map Text (DefPref p),
    -- | Inlines by name (open string)
    _inlines :: Map Text (DefInline p),
    -- | Rules by Id (int identifier)
    _rules :: Map Id [RuleTerm p],
    -- | Rules prepared for tokenizing
    _tokMap :: TokenizeMap
  }

type Token = Tok.Token Id Char (Id, Map VarName String)

type TokenizeMap = Tok.TokenizeMap Id Char (Id, Map VarName String)

deriving instance Box p => Show (Definitions p)

deriving instance Box p => Show (InModeDefs p)

instance Box p => Semigroup (Definitions p) where
  Definitions a b c <> Definitions a' b' c' = Definitions (a <> a') (b <> b') (c <> c')

instance Box p => Monoid (Definitions p) where
  mempty = Definitions mempty mempty mempty

instance Box p => Semigroup (InModeDefs p) where
  InModeDefs a b c d e <> InModeDefs a' b' c' d' e' =
    InModeDefs (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Box p => Monoid (InModeDefs p) where
  mempty = InModeDefs mempty mempty mempty mempty mempty

makeLenses ''Definitions
makeLenses ''InModeDefs
