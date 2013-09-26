{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Resource.Internal
    ( Resource (Resource)
    , resource
    , resource'
    , with
    , forkWith
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow (first)
import           Control.Applicative (Applicative, pure, (<*>))
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
                     , mtry
                     , onException
                     )


-- transformers---------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Class as T (MonadTrans, lift)


------------------------------------------------------------------------------
newtype Resource m a = Resource (m (a, m (), m ()))


------------------------------------------------------------------------------
instance Monad m => Functor (Resource m) where
    fmap f (Resource m) = Resource $ liftM (\(a, r, s) -> (f a, r, s)) m
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance MonadTry m => Applicative (Resource m) where
    pure a = Resource (return (a, return (), return ()))
    {-# INLINE pure #-}

    Resource mf <*> Resource ma = Resource $ mask $ \restore -> do
        (f, fr, fs) <- restore mf
        flip onException fr $ do
            (a, ar, as) <- restore ma
            return (f a, finally ar fr, onException as fr >> fs)
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadTry m => Monad (Resource m) where
    return a = Resource (return (a, return (), return ()))
    {-# INLINE return #-}

    Resource ma >>= f = Resource $ mask $ \restore -> do
        (a, ar, as) <- restore ma
        flip onException ar $ do
            let Resource mb = f a
            (b, br, bs) <- restore mb
            return (b, onException br ar >> ar, onException bs ar >> as)
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
    layer = Resource . liftM (\a -> (a, return (), return ()))
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadLayerFunctor (Resource m) where
    layerMap f (Resource m) = Resource $
        f (liftM (\(a, r, s) -> (a, f r, f s)) m)
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
        f (liftM (\(a, r, s) -> (a, f r, f s)) m)
    {-# INLINE transMap #-}
#endif


------------------------------------------------------------------------------
instance T.MonadTrans Resource where
    lift = Resource . liftM (\a -> (a, return (), return ()))


------------------------------------------------------------------------------
instance (MonadIO m, MonadTry m) => MonadIO (Resource m) where
    liftIO = Resource . liftIO . fmap (\a -> (a, return (), return ()))


------------------------------------------------------------------------------
resource :: MonadTry m => m a -> (a -> m ()) -> Resource m a
resource open close = resource' open close close
{-# INLINE resource #-}


------------------------------------------------------------------------------
resource' :: MonadTry m => m a -> (a -> m ()) -> (a -> m ()) -> Resource m a
resource' open onFailure onSuccess = Resource $
    liftM (\a -> (a, onFailure a, onSuccess a)) open
{-# INLINE resource' #-}


------------------------------------------------------------------------------
with :: MonadTry m => Resource m a -> (a -> m b) -> m b
with (Resource m) f = mask $ \restore -> restore m >>= \(a, r, s) ->
    onException (restore (f a)) r >>= \b -> s >> return b
{-# INLINABLE with #-}


------------------------------------------------------------------------------
forkWith :: (MonadTry m, MonadFork m)
    => Resource m a
    -> (a -> m ())
    -> m ThreadId
forkWith (Resource m) f = m >>= \(a, r, s) -> mask $ \restore -> fork $
    mtry (restore (f a)) >>= either (const r) (const s)
{-# INLINABLE forkWith #-}
