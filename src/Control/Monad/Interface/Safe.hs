{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Safe
    ( MonadSafe (register)
    , acquire
    , unsafeAcquire
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer
                     , type Inner
                     , layer
                     , MonadLift
                     , lift
                     )
import           Control.Monad.Interface.Mask (MonadMask, mask)


-- resource ------------------------------------------------------------------
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
class (Monad i, Monad m, MonadLift i m) => MonadSafe i m where
    register :: i () -> m (m ())


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadSafe i (Inner m)) =>
#else
instance (MonadLayer m, MonadSafe i (Inner m), MonadLift i m) =>
#endif
    MonadSafe i m
  where
    register = layer . liftM (layer) . register
    {-# INLINE register #-}


------------------------------------------------------------------------------
acquire :: (MonadSafe i m, MonadMask m) => Resource i a -> m (a, m ())
acquire (Resource m) = mask $ \unmask -> do
    (a, close) <- unmask (lift m)
    release <- register close
    return (a, release)
{-# INLINE acquire #-}


------------------------------------------------------------------------------
unsafeAcquire :: Monad m => Resource m a -> m (a, m ())
unsafeAcquire (Resource m) = m
{-# INLINE unsafeAcquire #-}
