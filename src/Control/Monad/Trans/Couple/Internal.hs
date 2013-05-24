{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Couple.Internal
    ( CoupleT (CoupleT)
    , couple
    , with
    , withFork
    , withForkOn
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow (first)
import           Control.Applicative (Applicative (pure, (<*>)))
import           Control.Concurrent (ThreadId)
import           Control.Monad
                     ( liftM
#if MIN_VERSION_base(4, 4, 0)
                     , liftM2
#endif
                     )
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip (mzip, mzipWith, munzip))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer, layerInvmap)
                     , MonadLayerFunctor (layerMap)
#if __GLASGOW_HASKELL__ >= 702
                     , MonadTrans (type Outer, transInvmap)
                     , MonadTransFunctor (transMap)
#endif
                     )
import           Control.Monad.Interface.Fork (MonadFork, fork, forkOn)
import           Control.Monad.Interface.Mask (mask)
import           Control.Monad.Interface.Try
                     ( MonadTry
                     , bracket
                     , finally
                     , onException
                     )


-- transformers---------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Class as T (MonadTrans (lift))


------------------------------------------------------------------------------
newtype CoupleT m a = CoupleT (m (a, m ()))


------------------------------------------------------------------------------
instance MonadTry m => Functor (CoupleT m) where
    fmap f (CoupleT m) = CoupleT $ liftM (first f) m
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance MonadTry m => Applicative (CoupleT m) where
    pure a = CoupleT (return (a, return ()))
    {-# INLINE pure #-}

    CoupleT mf <*> CoupleT ma = CoupleT $ mask $ \restore -> do
        (f, closef) <- restore mf
        flip onException closef $ do
            (a, closea) <- restore ma
            return (f a, onException closea closef >> closef)
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadTry m => Monad (CoupleT m) where
    return a = CoupleT (return (a, return ()))
    {-# INLINE return #-}

    CoupleT ma >>= f = CoupleT $ mask $ \restore -> do
        (a, closea) <- restore ma
        flip onException closea $ do
            let CoupleT mb = f a
            (b, closeb) <- restore mb
            return (b, onException closeb closea >> closea)
    {-# INLINE (>>=) #-}


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance (MonadTry m, MonadZip m) => MonadZip (CoupleT m) where
    mzipWith f = liftM2 f
    {-# INLINE mzipWith #-}
    mzip = liftM2 (,)
    {-# INLINE mzip #-}
    munzip m = (liftM fst m, liftM snd m)
    {-# INLINE munzip #-}
#endif


------------------------------------------------------------------------------
instance MonadTry m => MonadLayer (CoupleT m) where
    type Inner (CoupleT m) = m
    layer = CoupleT . liftM (\a -> (a, return ()))
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadLayerFunctor (CoupleT m) where
    layerMap f (CoupleT m) = CoupleT (f (liftM (fmap f) m))
    {-# INLINE layerMap #-}


#if __GLASGOW_HASKELL__ >= 702
------------------------------------------------------------------------------
instance MonadTry m => MonadTrans (CoupleT m) where
    type Outer (CoupleT m) = CoupleT
    transInvmap (f, _) = transMap f
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance MonadTry m => MonadTransFunctor (CoupleT m) where
    transMap f (CoupleT m) = CoupleT (f (liftM (fmap f) m))
    {-# INLINE transMap #-}
#endif


------------------------------------------------------------------------------
instance T.MonadTrans CoupleT where
    lift = CoupleT . liftM (\a -> (a, return ()))


------------------------------------------------------------------------------
instance (MonadIO m, MonadTry m) => MonadIO (CoupleT m) where
    liftIO = CoupleT . liftIO . fmap (\a -> (a, return ()))


------------------------------------------------------------------------------
couple :: MonadTry m => m a -> (a -> m ()) -> CoupleT m a
couple open close = CoupleT $ liftM (\a -> (a, close a)) open
{-# INLINE couple #-}


------------------------------------------------------------------------------
with :: MonadTry m
    => CoupleT m a
    -> (a -> m b)
    -> m b
with (CoupleT m) = bracket m snd . (. fst)
{-# INLINE with #-}


------------------------------------------------------------------------------
withFork :: (MonadTry m, MonadFork m)
    => CoupleT m a
    -> (a -> m ())
    -> m ThreadId
withFork (CoupleT m) f = mask $ \restore -> do
    restore m >>= \(a, close) -> fork $ restore (f a) `finally` close


------------------------------------------------------------------------------
withForkOn :: (MonadTry m, MonadFork m)
    => CoupleT m a
    -> Int
    -> (a -> m ())
    -> m ThreadId
withForkOn (CoupleT m) n f = mask $ \restore -> do
    restore m >>= \(a, close) -> forkOn n $ restore (f a) `finally` close
