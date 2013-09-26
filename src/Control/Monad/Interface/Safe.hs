module Control.Monad.Interface.Safe
    ( MonadSafe
    , register
    , register'
    , ReleaseKey
    , release
    , cancel
    , acquire
    , bracket
    , bracket_
    , bracketOnError
    , finally
    , onException
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Layer (MonadLift, lift)
import           Control.Monad.Interface.Mask (MonadMask, mask_)


-- resource ------------------------------------------------------------------
import           Control.Monad.Interface.Safe.Internal
                     ( MonadSafe
                     , register
                     , register'
                     , ReleaseKey
                     , release
                     , cancel
                     , bracket
                     , bracket_
                     , bracketOnError
                     , finally
                     , onException
                     )
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
acquire :: (MonadSafe i m, MonadMask m) => Resource i a -> m (a, ReleaseKey i)
acquire (Resource m) = mask_ $ do
    (a, fin) <- lift m
    key <- register' fin
    return (a, key)
{-# INLINE acquire #-}
