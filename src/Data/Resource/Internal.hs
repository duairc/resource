{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Resource.Internal
    ( Resource (Resource)
    , resource
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
                     layerMap
#if __GLASGOW_HASKELL__ >= 702
                     , MonadTrans
                     , type Outer
                     , transInvmap
                     , MonadTransFunctor
                     , transMap
#endif
                     )
import           Control.Monad.Interface.Fork (MonadFork, forkFinally)
import           Control.Monad.Interface.Mask (mask)
import           Control.Monad.Interface.Try
                     ( MonadTry
                     , bracket
                     , finally
                     , onException
                     )


-- transformers---------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Class as T (MonadTrans, lift)


------------------------------------------------------------------------------
newtype Resource m a = Resource (m (a, m ()))


------------------------------------------------------------------------------
instance MonadTry m => Functor (Resource m) where
    fmap f (Resource m) = Resource $ liftM (first f) m
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance MonadTry m => Applicative (Resource m) where
    pure a = Resource (return (a, return ()))
    {-# INLINE pure #-}

    Resource mf <*> Resource ma = Resource $ mask $ \restore -> do
        (f, closef) <- restore mf
        flip onException closef $ do
            (a, closea) <- restore ma
            return (f a, onException closea closef >> closef)
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadTry m => Monad (Resource m) where
    return a = Resource (return (a, return ()))
    {-# INLINE return #-}

    Resource ma >>= f = Resource $ mask $ \restore -> do
        (a, closea) <- restore ma
        flip onException closea $ do
            let Resource mb = f a
            (b, closeb) <- restore mb
            return (b, onException closeb closea >> closea)
    {-# INLINE (>>=) #-}


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance (MonadTry m, MonadZip m) => MonadZip (Resource m) where
    mzipWith f = liftM2 f
    {-# INLINE mzipWith #-}
    mzip = liftM2 (,)
    {-# INLINE mzip #-}
    munzip m = (liftM fst m, liftM snd m)
    {-# INLINE munzip #-}
#endif


------------------------------------------------------------------------------
instance MonadTry m => MonadLayer (Resource m) where
    type Inner (Resource m) = m
    layer = Resource . liftM (\a -> (a, return ()))
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadLayerFunctor (Resource m) where
    layerMap f (Resource m) = Resource (f (liftM (fmap f) m))
    {-# INLINE layerMap #-}


#if __GLASGOW_HASKELL__ >= 702
------------------------------------------------------------------------------
instance MonadTry m => MonadTrans (Resource m) where
    type Outer (Resource m) = Resource
    transInvmap (f, _) = transMap f
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadTransFunctor (Resource m) where
    transMap f (Resource m) = Resource (f (liftM (fmap f) m))
    {-# INLINE transMap #-}
#endif


------------------------------------------------------------------------------
instance T.MonadTrans Resource where
    lift = Resource . liftM (\a -> (a, return ()))


------------------------------------------------------------------------------
instance (MonadIO m, MonadTry m) => MonadIO (Resource m) where
    liftIO = Resource . liftIO . fmap (\a -> (a, return ()))


------------------------------------------------------------------------------
resource :: MonadTry m => m a -> (a -> m ()) -> Resource m a
resource open close = Resource $ liftM (\a -> (a, close a)) open
{-# INLINE resource #-}


------------------------------------------------------------------------------
with :: MonadTry m
    => Resource m a
    -> (a -> m b)
    -> m b
with (Resource m) = bracket m snd . (. fst)
{-# INLINE with #-}


------------------------------------------------------------------------------
forkWith :: (MonadTry m, MonadFork m)
    => Resource m a
    -> (a -> m ())
    -> m ThreadId
forkWith (Resource m) f = m >>= \(a, close) -> forkFinally (f a) (const close)
{-# INLINABLE forkWith #-}
