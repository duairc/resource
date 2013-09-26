{-# LANGUAGE BangPatterns #-}
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
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer
                     , type Inner
                     , layer
                     , MonadLift
                     , lift
                     )


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
data ReleaseKey m = ReleaseKey !(m ()) !(m ())


------------------------------------------------------------------------------
release :: MonadLift i m => ReleaseKey i -> m ()
release (ReleaseKey r s) = lift s
{-# INLINE release #-}


------------------------------------------------------------------------------
release' :: MonadLift i m => ReleaseKey i -> m ()
release' (ReleaseKey r s) = lift r
