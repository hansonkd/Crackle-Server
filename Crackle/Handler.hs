{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crackle.Handler where
    
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.CatchIO
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Category
import           Snap.Core
import           Prelude hiding ((.), catch)

------------------------------------------------------------------------------
--- Handler
    
newtype CrackleHandler a m c = CrackleHandler { runCrackle :: (a -> m (c, a))}

instance Functor m => Functor (CrackleHandler b m) where
    fmap f (CrackleHandler g) = CrackleHandler $ \v ->
        (\(a,v) -> (f a, v)) <$> g v

instance (Functor m, Monad m) => Applicative (CrackleHandler a m) where
    pure a = CrackleHandler $ \s -> return (a, s)
    CrackleHandler mf <*> CrackleHandler ma = CrackleHandler $ \a -> do
        (f, s) <- mf a
        (\(a, s'') -> (f a, s'')) <$> ma s

instance Monad m => Monad (CrackleHandler a m) where
    return a = CrackleHandler $ \s -> return (a, s)
    CrackleHandler g >>= k = CrackleHandler $ \a -> do
        (r, s') <- g a
        runCrackle (k r) s'

instance Monad m => MonadState a (CrackleHandler a m) where
    get = CrackleHandler $ \v -> return (v, v)
    put v' = CrackleHandler $ \a -> return ((), v')
    
instance Monad m => MonadReader a (CrackleHandler a m) where
  ask = CrackleHandler $ \a -> return (a, a)
  
instance MonadTrans (CrackleHandler a) where
    lift m = CrackleHandler $ \v -> do
      res <- m
      return (res, v)

instance MonadIO m => MonadIO (CrackleHandler a m) where
  liftIO = lift . liftIO

instance MonadCatchIO m => MonadCatchIO (CrackleHandler a m) where
    catch (CrackleHandler m) f = CrackleHandler $ \a -> m a `catch` handler a
      where
        handler a e = let CrackleHandler h = f e
                          in h a

    block (CrackleHandler m)   = CrackleHandler $ \a-> block (m a)
    unblock (CrackleHandler m) = CrackleHandler $ \a -> unblock (m a)

instance MonadPlus m => MonadPlus (CrackleHandler a m) where
    mzero = lift mzero
    m `mplus` n = CrackleHandler $ \a ->
                  runCrackle m a `mplus` runCrackle n a

instance (Monad m, Alternative m) => Alternative (CrackleHandler a m) where
    empty = lift empty
    CrackleHandler m <|> CrackleHandler n = CrackleHandler $ \a -> m a <|> n a

instance MonadSnap m => MonadSnap (CrackleHandler a m) where
    liftSnap = lift . liftSnap