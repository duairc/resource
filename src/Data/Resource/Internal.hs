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
import           Control.Applicative (Applicative, pure, (<*>))
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
import           Data.Monoid (Monoid, mempty, mappend)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer
                     , type Inner
                     , layer
                     , layerInvmap
                     , MonadLayerFunctor
                     , layerMap
#if __GLASGOW_HASKELL__ >= 702
                     , MonadTrans
                     , type Outer
                     , transInvmap
                     , MonadTransFunctor
                     , transMap
#endif
                     )
import           Control.Monad.Interface.Fork (MonadFork, fork)
import           Control.Monad.Interface.Mask (mask)
import           Control.Monad.Interface.Try
                     ( MonadTry
                     , finally
                     , onException
                     )


-- transformers---------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Class as T (MonadTrans, lift)


------------------------------------------------------------------------------
data Finalizers m = Finalizers { onError :: !(m ()), onSuccess :: !(m ()) }


------------------------------------------------------------------------------
instance MonadTry m => Monoid (Finalizers m) where
    mempty = Finalizers (return ()) (return ())
    Finalizers e s `mappend` Finalizers e' s' = Finalizers
        (finally e e')
        (onException s e' >> s')


------------------------------------------------------------------------------
newtype Resource m a = Resource { unsafeAcquire :: m (a, Finalizers m) }


------------------------------------------------------------------------------
instance Monad m => Functor (Resource m) where
    fmap f (Resource m) = Resource $ liftM (first f) m
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance MonadTry m => Applicative (Resource m) where
    pure a = Resource (return (a, mempty))
    {-# INLINE pure #-}

    Resource mf <*> Resource ma = Resource $ do
        (f, fin_f) <- mf
        (a, fin_a) <- ma `onException` onError fin_f
        return (f a, fin_a `mappend` fin_f)
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadTry m => Monad (Resource m) where
    return a = Resource (return (a, mempty))
    {-# INLINE return #-}

    Resource ma >>= f = Resource $ do
        (a, fin_a) <- ma
        let Resource mb = f a
        (b, fin_b) <- mb `onException` onError fin_a
        return (b, fin_b `mappend` fin_a)
    {-# INLINE (>>=) #-}

    fail = layer . fail
    {-# INLINE fail #-}


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance (MonadTry m, MonadZip m) => MonadZip (Resource m) where
    mzipWith = liftM2
    {-# INLINE mzipWith #-}
    mzip = liftM2 (,)
    {-# INLINE mzip #-}
    munzip m = (liftM fst m, liftM snd m)
    {-# INLINE munzip #-}
#endif


------------------------------------------------------------------------------
instance MonadTry m => MonadLayer (Resource m) where
    type Inner (Resource m) = m
    layer = Resource . liftM (\a -> (a, mempty))
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadLayerFunctor (Resource m) where
    layerMap f (Resource m) = Resource $
        f (liftM (\(a, Finalizers e s) -> (a, Finalizers (f e) (f s))) m)
    {-# INLINE layerMap #-}


#if __GLASGOW_HASKELL__ >= 702
------------------------------------------------------------------------------
instance MonadTry m => MonadTrans (Resource m) where
    type Outer (Resource m) = Resource
    transInvmap (f, _) = transMap f
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadTransFunctor (Resource m) where
    transMap f (Resource m) = Resource $
        f (liftM (\(a, Finalizers e s) -> (a, Finalizers (f e) (f s))) m)
    {-# INLINE transMap #-}
#endif


------------------------------------------------------------------------------
instance T.MonadTrans Resource where
    lift = Resource . liftM (\a -> (a, Finalizers (return ()) (return ())))


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
