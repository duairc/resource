{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Safe.Internal
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
import           Control.Monad.Lift (MonadTrans, lift, MonadLift, lift')
import           Monad.Mask (MonadMask, mask, mask_)


-- resource ------------------------------------------------------------------
import           Data.Resource.Internal (Finalizers (Finalizers, onSuccess))


------------------------------------------------------------------------------
class MonadLift i m => MonadSafe i m where
    register' :: Finalizers i -> m (ReleaseKey i)


------------------------------------------------------------------------------
instance (MonadTrans t, MonadSafe i m, MonadLift i (t m)) =>
    MonadSafe i (t m)
  where
    register' = lift . register'


------------------------------------------------------------------------------
register :: MonadSafe i m => i () -> m (ReleaseKey i)
register m = register' (Finalizers m m)


------------------------------------------------------------------------------
newtype ReleaseKey m = ReleaseKey (m (Finalizers m))


------------------------------------------------------------------------------
release :: (MonadLift i m, MonadMask m) => ReleaseKey i -> m ()
release (ReleaseKey m) = mask_ $ lift' $ m >>= onSuccess


------------------------------------------------------------------------------
cancel :: MonadLift i m => ReleaseKey i -> m ()
cancel (ReleaseKey m) = lift' $ liftM (const ()) m


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.bracket' from "Monad.Try", except this also
-- protects against premature termination (e.g., when using @pipes@).
bracket :: (MonadMask m, MonadSafe i m)
    => i a
    -> (a -> i b)
    -> (a -> m c)
    -> m c
bracket before after run = mask $ \unmask -> do
    a <- lift' before
    unmask (run a) `finally` after a
{-# INLINABLE bracket #-}


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.bracket_' from "Monad.Try", except this also
-- protects against premature termination (e.g., when using @pipes@).
bracket_ :: (MonadMask m, MonadSafe i m) => i a -> i b -> m c -> m c
bracket_ prequel sequel m = bracket prequel (\_ -> sequel) (\_ -> m)
{-# INLINABLE bracket_ #-}


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.bracketOnError' from "Monad.Try", except this
-- also protects against premature termination (e.g., when using @pipes@).
bracketOnError :: (MonadMask m, MonadSafe i m)
    => i a
    -> (a -> i b)
    -> (a -> m c)
    -> m c
bracketOnError before after run = mask $ \unmask -> do
    a <- lift' before
    unmask (run a) `onException` after a
{-# INLINABLE bracketOnError #-}


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.finally' from "Monad.Try", except this also
-- protects against premature termination (e.g., when using @pipes@).
finally :: (MonadMask m, MonadSafe i m) => m a -> i b -> m a
finally m sequel = do
    key <- register (sequel >> return ())
    a <- m
    release key
    return a
{-# INLINABLE finally #-}


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.onException' from "Monad.Try", except this also
-- protects against premature termination (e.g., when using @pipes@).
onException :: MonadSafe i m => m a -> i b -> m a
onException m sequel = do
    key <- register (sequel >> return ())
    a <- m
    cancel key
    return a
{-# INLINABLE onException #-}
