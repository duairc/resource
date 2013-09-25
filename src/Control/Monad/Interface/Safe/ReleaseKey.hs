module Control.Monad.Interface.Safe.ReleaseKey
    ( ReleaseKey (ReleaseKey)
    )
where

------------------------------------------------------------------------------
newtype ReleaseKey m = ReleaseKey (m ())
