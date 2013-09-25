{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Safe
    ( MonadSafe (register)
    , ReleaseKey
    , acquire
    , release
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
import           Control.Monad.Interface.Safe.ReleaseKey
                     ( ReleaseKey (ReleaseKey)
                     )
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
class (Monad i, Monad m, MonadLift i m) => MonadSafe i m where
    register :: i () -> m (ReleaseKey i)


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadSafe i (Inner m)) =>
#else
instance (MonadLayer m, MonadSafe i (Inner m), MonadLift i m) =>
#endif
    MonadSafe i m
  where
    register = layer . register
    {-# INLINE register #-}


------------------------------------------------------------------------------
acquire :: (MonadSafe i m, MonadMask m) => Resource i a -> m (a, ReleaseKey i)
acquire (Resource m) = mask $ \unmask -> do
    (a, close) <- unmask (lift m)
    key <- register close
    return (a, key)
{-# INLINE acquire #-}


------------------------------------------------------------------------------
release :: MonadLift i m => ReleaseKey i -> m ()
release (ReleaseKey m) = lift m
{-# INLINE release #-}


------------------------------------------------------------------------------
unsafeAcquire :: MonadLift i m => Resource i a -> m (a, ReleaseKey i)
unsafeAcquire (Resource m) = lift $ liftM (\(a, r) -> (a, ReleaseKey r)) m
{-# INLINE unsafeAcquire #-}
