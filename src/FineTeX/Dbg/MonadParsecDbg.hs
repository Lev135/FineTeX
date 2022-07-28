module FineTeX.Dbg.MonadParsecDbg where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Writer (WriterT (..))
import Text.Megaparsec (ParsecT, ShowErrorComponent, VisualStream)
import qualified Text.Megaparsec.Debug

class Monad m => MonadParsecDbg m where
  dbg :: Show a => String -> m a -> m a

instance (VisualStream s, ShowErrorComponent e) => MonadParsecDbg (ParsecT e s m) where
  dbg = Text.Megaparsec.Debug.dbg

instance (Show s, MonadParsecDbg m) => MonadParsecDbg (StateT s m) where
  dbg str sma = StateT $ \s ->
    dbg str $ runStateT sma s

instance (Show w, Monoid w, MonadParsecDbg m) => MonadParsecDbg (WriterT w m) where
  dbg str wma = WriterT $ dbg str $ runWriterT wma

instance (MonadParsecDbg m) => MonadParsecDbg (ReaderT e m) where
  dbg str rma = ReaderT $ \e -> dbg str $ runReaderT rma e
