module Data.Resource
    ( Resource (Resource, unsafeAcquire)
    , resource
    , resource'
    , with
    , forkWith
    , MonadSafe
    , register
    , register'
    , acquire
    , ReleaseKey
    , release
    , cancel
    , SafeT
    , runSafeT
    )
where

-- resource ------------------------------------------------------------------
import           Control.Monad.Interface.Safe
                     ( MonadSafe
                     , register
                     , register'
                     , acquire
                     , ReleaseKey
                     , release
                     , cancel
                     )
import           Control.Monad.Trans.Safe (SafeT, runSafeT)
import           Data.Resource.Internal
                     ( Resource (Resource, unsafeAcquire)
                     , resource
                     , resource'
                     , with
                     , forkWith
                     )
