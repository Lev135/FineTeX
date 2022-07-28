{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FineTeX.Processor.Definitions
  ( ProcessDefsError,
    processDefs,
  )
where

import Control.Lens (At (at), Field1 (_1), Lens', non, to, use, (%=), (^.))
import Control.Monad (forM_, when)
import Control.Monad.RWS (MonadState, MonadWriter (tell))
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import FineTeX.Parser.Syntax
import FineTeX.Processor.Syntax
import FineTeX.Utils (Box (unBox), Pos, PosC (..), PrettyErr (..), prettyPos, sequenceBox)
import qualified Prettyprinter as P

-- Processing definitions

data ProcessDefsError
  = MultipleDecl DefType Text Pos Pos
  | UndefinedMode Text Pos
  deriving (Show)

instance PrettyErr ProcessDefsError where
  prettyErr src (MultipleDecl t name p p') =
    P.vsep
      [prettyPos src lbl p, prettyPos src "Previously defined here" p']
    where
      lbl = "Multiple definitions of " <> tName <> " '" <> name <> "'"
      tName = case t of
        DefTEnv -> "environment"
        DefTCmd -> "command"
        DefTInl -> "inline"
        DefTMode -> "mode"
        DefTPref -> "pref"
  prettyErr src (UndefinedMode name p) = prettyPos src lbl p
    where
      lbl = "Undefined mode '" <> name <> "'"

type MState m p =
  (MonadWriter [ProcessDefsError] m, MonadState (Definitions p) m)

data DefType = DefTEnv | DefTPref | DefTCmd | DefTInl | DefTMode
  deriving (Show)

-- | Processing definitions.
--   Initial state should contain definitions, imported from other files
processDefs ::
  forall m p.
  (PosC p, MonadWriter [ProcessDefsError] m, MonadState (Definitions p) m) =>
  DefBlock p ->
  m ()
processDefs defs = do
  -- Lay out definitions in appropriate maps and check conflicting names
  procDefBlock defs
  inModeModes <- use $ inModes . to (map (second getPos) . M.toList)
  -- Check modes for which we found definitions to be defined
  forM_ inModeModes $ \(pname, p) -> do
    mmode <- use $ modes . at pname
    when (isNothing mmode) $ tell [UndefinedMode pname p]

-- TODO: make check for uniquely parsing of commands (rename ~~> tokens? prims?)

procDefBlock :: forall m p. (PosC p, MState m p) => DefBlock p -> m ()
procDefBlock = mapM_ $ \case
  DefModeBlock defs -> mapM_ procDefMode defs
  DefInModeBlock mode defs -> mapM_ (procDefInModeBlock mode) defs

procDefMode :: forall m p. (PosC p, MState m p) => DefMode p -> m ()
procDefMode = addDef modes DefTMode

instance HasName (DefInline p) (p Text) where
  name = borders . _1

procDefInModeBlock ::
  forall m p. (PosC p, MState m p) => p Text -> DefInModeBlock p -> m ()
procDefInModeBlock mode = \case
  DefEnvBlock defs -> mapM_ (addInMode envs DefTEnv) defs
  DefCmdBlock defs -> mapM_ (addInMode cmds DefTCmd) defs
  DefPrefBlock defs -> mapM_ (addInMode prefs DefTPref) defs
  DefInlBlock defs -> mapM_ (addInMode inlines DefTInl) defs
  where
    addInMode ::
      HasName (a p) (p Text) =>
      Lens' (InModeDefs p) (Map Text (a p)) ->
      DefType ->
      (a p -> m ())
    addInMode lens deft = addDef (inModes . at (unBox mode) . non (mempty <$ mode) . h . lens) deft
    h g pa = sequenceBox $ g <$> pa

addDef ::
  forall m p a.
  (PosC p, MState m p, HasName (a p) (p Text)) =>
  Lens' (Definitions p) (Map Text (a p)) ->
  DefType ->
  a p ->
  m ()
addDef lens deft val = do
  mval' <- use $ lens . at (val ^. name . to unBox)
  case mval' of
    Nothing -> lens %= M.insert (val ^. name . to unBox) val
    Just val' ->
      tell [MultipleDecl deft (unBox $ val ^. name) (getPos $ val ^. name) (getPos $ val' ^. name)]
