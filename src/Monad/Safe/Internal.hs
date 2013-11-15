{-# LANGUAGE CPP #-}
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


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadInner, liftI)
import           Control.Monad.Lift.Top (MonadTop, liftT)
import           Monad.Mask (MonadMask, mask, mask_)


-- transformers --------------------------------------------------------------
import           Data.Functor.Product (Product (Pair))


-- resource ------------------------------------------------------------------
import           Data.Resource.Internal (Finalizers (Finalizers, onSuccess))


------------------------------------------------------------------------------
class MonadInner i m => MonadSafe i m where
    register' :: Finalizers i -> m (ReleaseKey i)


------------------------------------------------------------------------------
instance (MonadSafe i f, MonadSafe i g) => MonadSafe i (Pair f g) where
    register' f = Pair (register' f) (register' f)


------------------------------------------------------------------------------
instance MonadSafe i (f (g m)) => MonadSafe i (ComposeT f g m) where
    register' = ComposeT . register'


------------------------------------------------------------------------------
instance (MonadTop t m, MonadSafe i m, MonadInner i (t m)) =>
    MonadSafe i (t m)
  where
    register' = liftT . register'


------------------------------------------------------------------------------
register :: MonadSafe i m => i () -> m (ReleaseKey i)
register m = register' (Finalizers m m)


------------------------------------------------------------------------------
newtype ReleaseKey m = ReleaseKey (m (Finalizers m))


------------------------------------------------------------------------------
release :: (MonadInner i m, MonadMask m) => ReleaseKey i -> m ()
release (ReleaseKey m) = mask_ $ liftI $ m >>= onSuccess


------------------------------------------------------------------------------
cancel :: MonadInner i m => ReleaseKey i -> m ()
cancel (ReleaseKey m) = liftI $ liftM (const ()) m


------------------------------------------------------------------------------
-- | Analogous to 'Monad.Try.bracket' from "Monad.Try", except this also
-- protects against premature termination (e.g., when using @pipes@).
bracket :: (MonadMask m, MonadSafe i m)
    => i a
    -> (a -> i b)
    -> (a -> m c)
    -> m c
bracket before after run = mask $ \restore -> do
    a <- liftI before
    restore (run a) `finally` after a
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
bracketOnError before after run = mask $ \restore -> do
    a <- liftI before
    restore (run a) `onException` after a
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
