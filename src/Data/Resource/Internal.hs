{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Resource.Internal
    ( Resource (Resource, unsafeAcquire)
    , Finalizers (Finalizers, onError, onSuccess)
    , resource
    , resource'
    , with
    , forkWith
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure, (<*>))
#endif
import           Control.Arrow (first)
import           Control.Concurrent (ThreadId)
import           Control.Monad
                     ( liftM
#if MIN_VERSION_base(4, 4, 0)
                     , liftM2
#endif
                     )
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip, mzip, mzipWith, munzip)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mempty, mappend)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans
                     , lift
                     , MInvariant
                     , hoistiso
                     , MFunctor
                     , hoist
                     )
import           Monad.Fork (MonadFork, fork)
import           Monad.Mask (mask)
import           Monad.Try (MonadTry, finally, onException)


-- transformers---------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)


------------------------------------------------------------------------------
data Finalizers m = Finalizers { onError :: !(m ()), onSuccess :: !(m ()) }


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance MonadTry m => Semigroup (Finalizers m) where
    Finalizers e s <> Finalizers e' s' = Finalizers
        (finally e e')
        (onException s e' >> s')


#endif
------------------------------------------------------------------------------
instance MonadTry m => Monoid (Finalizers m) where
    mempty = Finalizers (return ()) (return ())
#if MIN_VERSION_base(4, 9, 0)
    mappend = (<>)
#else
    Finalizers e s `mappend` Finalizers e' s' = Finalizers
        (finally e e')
        (onException s e' >> s')
#endif


------------------------------------------------------------------------------
newtype Resource m a = Resource { unsafeAcquire :: m (a, Finalizers m) }


------------------------------------------------------------------------------
instance MonadTrans Resource where
    lift = Resource . liftM (\a -> (a, Finalizers (return ()) (return ())))


------------------------------------------------------------------------------
instance MInvariant Resource where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MFunctor Resource where
    hoist f (Resource m) = Resource $
        f (liftM (\(a, Finalizers e s) -> (a, Finalizers (f e) (f s))) m)


------------------------------------------------------------------------------
instance Functor m => Functor (Resource m) where
    fmap f (Resource m) = Resource $ fmap (first f) m


------------------------------------------------------------------------------
instance (Functor m, MonadTry m) => Applicative (Resource m) where
    pure a = Resource (return (a, mempty))
    Resource mf <*> Resource ma = Resource $ do
        (f, fin_f) <- mf
        (a, fin_a) <- ma `onException` onError fin_f
        return (f a, fin_a `mappend` fin_f)


------------------------------------------------------------------------------
instance MonadTry m => Monad (Resource m) where
    return a = Resource (return (a, mempty))
    Resource ma >>= f = Resource $ do
        (a, fin_a) <- ma
        let Resource mb = f a
        (b, fin_b) <- mb `onException` onError fin_a
        return (b, fin_b `mappend` fin_a)
    fail = lift . fail


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance (MonadTry m, MonadZip m) => MonadZip (Resource m) where
    mzipWith = liftM2
    mzip = liftM2 (,)
    munzip m = (liftM fst m, liftM snd m)
#endif


------------------------------------------------------------------------------
instance (MonadIO m, MonadTry m) => MonadIO (Resource m) where
    liftIO = Resource . liftIO . fmap (\a -> (a, mempty))


------------------------------------------------------------------------------
resource :: MonadTry m => m a -> (a -> m ()) -> Resource m a
resource open close = resource' open (\a -> Finalizers (close a) (close a))
{-# INLINE resource #-}


------------------------------------------------------------------------------
resource' :: MonadTry m => m a -> (a -> Finalizers m) -> Resource m a
resource' open finalizers = Resource $ liftM (\a -> (a, finalizers a)) open
{-# INLINE resource' #-}


------------------------------------------------------------------------------
with :: MonadTry m => Resource m a -> (a -> m b) -> m b
with (Resource m) f = mask $ \restore -> m >>= \(a, Finalizers e s) ->
    onException (restore (f a)) e >>= \b -> s >> return b
{-# INLINABLE with #-}


------------------------------------------------------------------------------
forkWith :: (MonadTry m, MonadFork m)
    => Resource m a
    -> (a -> m ())
    -> m ThreadId
forkWith (Resource m) f = mask $ \restore -> m >>= \(a, Finalizers e s) ->
    fork $ onException (restore (f a)) e >> s
{-# INLINABLE forkWith #-}
