{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dbg
  ( ReaderT (..),
    WriterT (..),
    StateT (..),
    runReader,
    runWriter,
    runState,
    unBox,
    p,
    Definitions,
    Posed,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.String (IsString (..))
import qualified Data.Text as T
import FineTeX.FineTeX
import FineTeX.Parser.Utils
import FineTeX.Utils

instance s ~ T.Text => IsString (Posed s) where
  fromString str = Posed (T.pack str) undefined

p :: a -> Posed a
p a = Posed a undefined
