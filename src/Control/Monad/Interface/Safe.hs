{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Safe
    ( MonadSafe (acquire)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer)
                     , MonadLift
                     )


-- resource ------------------------------------------------------------------
import           Data.Resource (Resource)


------------------------------------------------------------------------------
class (Monad i, Monad m, MonadLift i m) => MonadSafe i m where
    acquire :: Resource i a -> m (a, m ())


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadSafe i (Inner m)) =>
#else
instance (MonadLayer m, MonadSafe i (Inner m), MonadLift i m) =>
#endif
    MonadSafe i m
  where
    acquire = layer . liftM (fmap layer) . acquire
    {-# INLINE acquire #-}
