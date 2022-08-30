{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FineTeX.Processor.Syntax where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Text (Text)
import FineTeX.Parser.Syntax
import FineTeX.Parser.Utils (Posed)
import FineTeX.Processor.Tokenizer (BlackWhiteSetList)
import qualified FineTeX.Processor.Tokenizer as Tok
import Prelude hiding (Word)

data Definitions = Definitions
  { _modes :: Map Text DefMode,
    -- | Sorts tokens by name
    _sorts :: Map Text (Posed [BlackWhiteSetList Char]),
    _inModes :: Map Text (Posed InModeDefs)
  }
  deriving (Show)

type Id = Int

data InModeDefs = InModeDefs
  { -- | Environments by name
    _envs :: Map Text DefEnvironment,
    -- | Prefs by name (pref string)
    _prefs :: Map Text DefPref,
    -- | Inlines by name (open string)
    _inlines :: Map Text DefInline,
    -- | Rules by Id (int identifier)
    _rules :: Map Id [RuleTerm],
    -- | Rules prepared for tokenizing
    _tokMap :: TokenizeMap
  }
  deriving (Show)

type Token = Tok.Token Id Char (Id, Map VarName String)

type TokenizeMap = Tok.TokenizeMap Id Char (Id, Map VarName String)

instance Semigroup Definitions where
  Definitions a b c <> Definitions a' b' c' = Definitions (a <> a') (b <> b') (c <> c')

instance Monoid Definitions where
  mempty = Definitions mempty mempty mempty

instance Semigroup InModeDefs where
  InModeDefs a b c d e <> InModeDefs a' b' c' d' e' =
    InModeDefs (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid InModeDefs where
  mempty = InModeDefs mempty mempty mempty mempty mempty

makeLenses ''Definitions
makeLenses ''InModeDefs
