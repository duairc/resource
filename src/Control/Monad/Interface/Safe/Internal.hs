{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Safe.Internal
    ( MonadSafe (register')
    , register
    , ReleaseKey (ReleaseKey)
    , release
    , release'
    , cancel
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
import           Control.Monad.Interface.Mask (MonadMask, mask_)


------------------------------------------------------------------------------
class MonadLift i m => MonadSafe i m where
    register' :: i () -> i () -> m (ReleaseKey i)


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadSafe i (Inner m)) =>
#else
instance (MonadLayer m, MonadSafe i (Inner m), MonadLift i m) =>
#endif
    MonadSafe i m
  where
    register' r s = layer (register' r s)
    {-# INLINE register' #-}


------------------------------------------------------------------------------
register :: MonadSafe i m => i () -> m (ReleaseKey i)
register m = register' m m
{-# INLINE register #-}


------------------------------------------------------------------------------
newtype ReleaseKey m = ReleaseKey (m (m (), m ()))


------------------------------------------------------------------------------
release :: (MonadLift i m, MonadMask i) => ReleaseKey i -> m ()
release (ReleaseKey m) = lift $ mask_ $ m >>= snd
{-# INLINE release #-}


------------------------------------------------------------------------------
release' :: (MonadLift i m, MonadMask i) => ReleaseKey i -> m ()
release' (ReleaseKey m) = lift $ mask_ $ m >>= fst
{-# INLINE release' #-}


------------------------------------------------------------------------------
cancel :: MonadLift i m => ReleaseKey i -> m ()
cancel (ReleaseKey m) = lift $ liftM (const ()) m
