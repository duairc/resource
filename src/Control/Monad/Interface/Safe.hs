module Control.Monad.Interface.Safe
    ( MonadSafe (register)
    , ReleaseKey
    , release
    , acquire
    , unsafeAcquire
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer (MonadLift, lift)
import           Control.Monad.Interface.Mask (MonadMask, mask)


-- resource ------------------------------------------------------------------
import           Control.Monad.Interface.Safe.Internal
                     ( MonadSafe
                     , register
                     , ReleaseKey (ReleaseKey)
                     , release
                     )
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
acquire :: (MonadSafe i m, MonadMask m) => Resource i a -> m (a, ReleaseKey i)
acquire (Resource m) = mask $ \unmask -> do
    (a, close) <- unmask (lift m)
    key <- register close
    return (a, key)
{-# INLINE acquire #-}


------------------------------------------------------------------------------
unsafeAcquire :: MonadLift i m => Resource i a -> m (a, ReleaseKey i)
unsafeAcquire (Resource m) = lift $ liftM (\(a, r) -> (a, ReleaseKey r)) m
{-# INLINE unsafeAcquire #-}
