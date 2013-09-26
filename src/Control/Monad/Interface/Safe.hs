module Control.Monad.Interface.Safe
    ( MonadSafe
    , register
    , register'
    , ReleaseKey
    , release
    , release'
    , acquire
    , unsafeAcquire
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer (MonadLift, lift)
import           Control.Monad.Interface.Mask (MonadMask, mask_)


-- resource ------------------------------------------------------------------
import           Control.Monad.Interface.Safe.Internal
                     ( MonadSafe
                     , register
                     , register'
                     , ReleaseKey (ReleaseKey)
                     , release
                     , release'
                     )
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
acquire :: (MonadSafe i m, MonadMask m) => Resource i a -> m (a, ReleaseKey i)
acquire (Resource m) = mask_ $ do
    (a, r, s) <- lift m
    key <- register' r s
    return (a, key)
{-# INLINE acquire #-}


------------------------------------------------------------------------------
unsafeAcquire :: MonadLift i m => Resource i a -> m (a, ReleaseKey i)
unsafeAcquire (Resource m) = lift $ liftM (\(a, r, s) -> (a, ReleaseKey r s)) m
{-# INLINE unsafeAcquire #-}
