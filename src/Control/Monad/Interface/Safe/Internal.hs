{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Safe.Internal
    ( MonadSafe (register)
    , ReleaseKey (ReleaseKey)
    , release
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
newtype ReleaseKey m = ReleaseKey (m ())


------------------------------------------------------------------------------
release :: MonadLift i m => ReleaseKey i -> m ()
release (ReleaseKey m) = lift m
{-# INLINE release #-}
