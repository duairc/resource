module Data.Resource
    ( Resource (Resource, unsafeAcquire)
    , Finalizers (Finalizers, onError, onSuccess)
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
import           Control.Monad.Trans.Safe (SafeT, runSafeT)
import           Data.Resource.Internal
                     ( Resource (..)
                     , Finalizers (..)
                     , resource
                     , resource'
                     , with
                     , forkWith
                     )
import           Monad.Safe
                     ( MonadSafe
                     , register
                     , register'
                     , acquire
                     , ReleaseKey
                     , release
                     , cancel
                     )
