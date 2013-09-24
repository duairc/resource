{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_Unsafe
{-# LANGUAGE Unsafe #-}
#endif

module Data.Resource.Unsafe
    ( unsafeAcquire
    )
where


-- resource ------------------------------------------------------------------
import           Data.Resource.Internal (Resource (Resource))


------------------------------------------------------------------------------
unsafeAcquire :: Monad m => Resource m a -> m (a, m ())
unsafeAcquire (Resource m) = m
{-# INLINE unsafeAcquire #-}
