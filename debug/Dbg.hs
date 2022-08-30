{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dbg
  ( ReaderT (..),
    WriterT (..),
    StateT (..),
    runReader,
    runWriter,
    runState,
    getVal,
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

instance s ~ T.Text => IsString (Posed s) where
  fromString str = Posed undefined (T.pack str)

p :: a -> Posed a
p = Posed undefined
