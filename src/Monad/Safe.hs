module Monad.Safe
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
import           Control.Monad.Lift (liftI)
import           Monad.Mask (MonadMask, mask_)


-- resource ------------------------------------------------------------------
import           Monad.Safe.Internal
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
    (a, fin) <- liftI m
    key <- register' fin
    return (a, key)
{-# INLINE acquire #-}
