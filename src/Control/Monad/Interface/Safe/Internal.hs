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
    , cancel
    , bracket
    , bracket_
    , bracketOnError
    , finally
    , onException
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer
                     , type Inner
                     , layer
                     , MonadLift
                     , lift
                     )
import           Control.Monad.Interface.Mask (MonadMask, mask, mask_)


-- resource ------------------------------------------------------------------
import           Data.Resource.Internal (Finalizers (Finalizers, onSuccess))


------------------------------------------------------------------------------
class MonadLift i m => MonadSafe i m where
    register' :: Finalizers i -> m (ReleaseKey i)


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadSafe i (Inner m)) =>
#else
instance (MonadLayer m, MonadSafe i (Inner m), MonadLift i m) =>
#endif
    MonadSafe i m
  where
    register' = layer . register'
    {-# INLINE register' #-}


------------------------------------------------------------------------------
register :: MonadSafe i m => i () -> m (ReleaseKey i)
register m = register' (Finalizers m m)
{-# INLINE register #-}


------------------------------------------------------------------------------
newtype ReleaseKey m = ReleaseKey (m (Finalizers m))


------------------------------------------------------------------------------
release :: (MonadLift i m, MonadMask m) => ReleaseKey i -> m ()
release (ReleaseKey m) = mask_ $ lift $ m >>= onSuccess
{-# INLINE release #-}


------------------------------------------------------------------------------
cancel :: MonadLift i m => ReleaseKey i -> m ()
cancel (ReleaseKey m) = lift $ liftM (const ()) m


------------------------------------------------------------------------------
-- | Analogous to 'Control.Monad.Interface.Try.bracket' from
-- "Control.Monad.Interface.Try", except this also protects against premature
-- termination (e.g., when using @pipes@).
bracket :: (MonadMask m, MonadSafe i m)
    => i a
    -> (a -> i b)
    -> (a -> m c)
    -> m c
bracket before after run = mask $ \unmask -> do
    a <- lift before
    unmask (run a) `finally` after a
{-# INLINABLE bracket #-}


------------------------------------------------------------------------------
-- | Analogous to 'Control.Monad.Interface.Try.bracket_' from
-- "Control.Monad.Interface.Try", except this also protects against premature
-- termination (e.g., when using @pipes@).
bracket_ :: (MonadMask m, MonadSafe i m) => i a -> i b -> m c -> m c
bracket_ prequel sequel m = bracket prequel (\_ -> sequel) (\_ -> m)
{-# INLINABLE bracket_ #-}


------------------------------------------------------------------------------
-- | Analogous to 'Control.Monad.Interface.Try.bracketOnError' from
-- "Control.Monad.Interface.Try", except this also protects against premature
-- termination (e.g., when using @pipes@).
bracketOnError :: (MonadMask m, MonadSafe i m)
    => i a
    -> (a -> i b)
    -> (a -> m c)
    -> m c
bracketOnError before after run = mask $ \unmask -> do
    a <- lift before
    unmask (run a) `onException` after a
{-# INLINABLE bracketOnError #-}


------------------------------------------------------------------------------
-- | Analogous to 'Control.Monad.Interface.Try.finally' from
-- "Control.Monad.Interface.Try", except this also protects against premature
-- termination (e.g., when using @pipes@).
finally :: (MonadMask m, MonadSafe i m) => m a -> i b -> m a
finally m sequel = do
    key <- register (sequel >> return ())
    a <- m
    release key
    return a
{-# INLINABLE finally #-}


------------------------------------------------------------------------------
-- | Analogous to 'Control.Monad.Interface.Try.onException' from
-- "Control.Monad.Interface.Try", except this also protects against premature
-- termination (e.g., when using @pipes@).
onException :: MonadSafe i m => m a -> i b -> m a
onException m sequel = do
    key <- register (sequel >> return ())
    a <- m
    cancel key
    return a
{-# INLINABLE onException #-}
