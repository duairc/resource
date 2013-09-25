{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Safe
    ( SafeT
    , runSafeT
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative
                     ( Applicative (pure, (<*>))
                     , Alternative (empty, (<|>))
                     )
import           Control.Monad
                     ( MonadPlus (mzero, mplus)
                     , ap
                     , liftM
#if MIN_VERSION_base(4, 4, 0)
                     , liftM2
#endif
                     , when
                     )
import           Control.Monad.Fix (MonadFix (mfix))
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip (mzip, mzipWith, munzip))
#endif
import           Data.Word (Word)


-- containers ----------------------------------------------------------------
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I


-- transformers --------------------------------------------------------------
import qualified Control.Monad.Trans.Class as T (MonadTrans (lift))
import           Control.Monad.IO.Class (MonadIO (liftIO))


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer
                     , type Inner
                     , layer
                     , layerInvmap
                     , MonadLayerFunctor
                     , layerMap
                     , MonadLayerControl
                     , type LayerState
                     , restore
                     , layerControl
#if __GLASGOW_HASKELL__ >= 702
                     , MonadTrans
                     , type Outer
                     , transInvmap
                     , MonadTransFunctor
                     , transMap
                     , MonadTransControl
                     , transControl
#endif
                     , MonadLift
                     , lift
                     , controlLayer
                     )
import           Control.Monad.Interface.Fork (MonadFork, fork, forkOn)
import           Control.Monad.Interface.Mask (MonadMask, mask, mask_)
import           Control.Monad.Interface.ST
                     ( MonadST
                     , atomicModifyRef'
                     , newRef
                     , writeRef
                     )
import           Control.Monad.Interface.Try (MonadTry, finally, mtry)


-- resource ------------------------------------------------------------------
import           Control.Monad.Interface.Safe.Internal
                     ( MonadSafe
                     , ReleaseKey (ReleaseKey)
                     )
import qualified Control.Monad.Interface.Safe.Internal (register)


------------------------------------------------------------------------------
data Finalizers i = Finalizers !Int !Word !(IntMap (i ()))


------------------------------------------------------------------------------
newtype SafeT v i m a = SafeT (v (Finalizers i) -> m a)


------------------------------------------------------------------------------
instance T.MonadTrans (SafeT v i) where
    lift = layer
    {-# INLINE lift #-}


------------------------------------------------------------------------------
instance Monad m => Functor (SafeT v i m) where
    fmap = liftM
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Monad m => Applicative (SafeT v i m) where
    pure = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (SafeT v i m) where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}


------------------------------------------------------------------------------
instance Monad m => Monad (SafeT v i m) where
    return = layer . return
    {-# INLINE return #-}
    SafeT m >>= f = SafeT $ \r -> m r >>= \a ->
        let SafeT m' = f a in m' r
    {-# INLINE (>>=) #-}
    fail = layer . fail
    {-# INLINE fail #-}


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (SafeT v i m) where
    mzero = layer mzero
    {-# INLINE mzero #-}
    mplus a b = controlLayer (\run -> mplus (run a) (run b))
    {-# INLINE mplus #-}


------------------------------------------------------------------------------
instance MonadFix m => MonadFix (SafeT v i m) where
    mfix f = controlLayer (\run -> mfix (\a -> run (restore a >>= f)))
    {-# INLINE mfix #-}


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance MonadZip m => MonadZip (SafeT v i m) where
    mzipWith f = liftM2 f
    {-# INLINE mzipWith #-}
    mzip = liftM2 (,)
    {-# INLINE mzip #-}
    munzip m = (liftM fst m, liftM snd m)
    {-# INLINE munzip #-}
#endif


------------------------------------------------------------------------------
instance MonadIO m => MonadIO (SafeT v i m) where
    liftIO = layer . liftIO
    {-# INLINE liftIO #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (SafeT v i m) where
    type Inner (SafeT v i m) = m
    layer = SafeT . const
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (SafeT v i m) where
    layerMap f (SafeT m) = SafeT $ f . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (SafeT v i m) where
    newtype LayerState (SafeT v i m) a = L {unL :: a}
    restore = SafeT . const . return . unL
    {-# INLINE restore #-}
    layerControl f = SafeT $ \r -> f $ \(SafeT t) -> liftM L $ t r
    {-# INLINE layerControl #-}


#if __GLASGOW_HASKELL__ >= 702
------------------------------------------------------------------------------
instance Monad m => MonadTrans (SafeT v i m) where
    type Outer (SafeT v i m) = SafeT v i
    transInvmap (f, _) = transMap f
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (SafeT v i m) where
    transMap f (SafeT m) = SafeT $ f . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (SafeT v i m) where
    transControl f = SafeT $ \r -> f $ \(SafeT t) -> liftM L $ t r
    {-# INLINE transControl #-}
#endif


------------------------------------------------------------------------------
instance 
    ( MonadFork m
    , MonadLift i m
    , MonadST v i
    , MonadTry i
    , MonadTry m
    )
  =>
    MonadFork (SafeT v i m)
  where
    fork (SafeT f) = SafeT $ \istate -> mask $ \unmask -> do
        lift $ stateAlloc istate
        fork $ unmask (f istate) `finally` lift (stateCleanup istate)

    forkOn n (SafeT f) = SafeT $ \istate -> mask $ \unmask -> do
        lift $ stateAlloc istate
        forkOn n $ unmask (f istate) `finally` lift (stateCleanup istate)


------------------------------------------------------------------------------
instance (MonadST v i, MonadLift i m, MonadMask i) =>
    MonadSafe i (SafeT v i m)
  where
    register close = SafeT $ \istate ->
        lift $ liftM ReleaseKey $ register istate close
    {-# INLINE register #-}


------------------------------------------------------------------------------
runSafeT :: (MonadST v i, MonadLift i m, MonadTry i, MonadTry m)
    => SafeT v i m a
    -> m a
runSafeT (SafeT f :: SafeT v i m a) = do
    istate <- lift $ (newRef $ Finalizers 0 0 I.empty :: i (v (Finalizers i)))
    mask $ \unmask -> do
        lift $ stateAlloc istate
        unmask (f istate) `finally` lift (stateCleanup istate)


------------------------------------------------------------------------------
register :: (MonadST v i, MonadMask i) => v (Finalizers i) -> i () -> i (i ())
register istate m = atomicModifyRef' istate $ \(Finalizers key ref im) ->
    (Finalizers (key + 1) ref (I.insert key m im), mask $ \unmask -> do
        action <- atomicModifyRef' istate (lookupAction key)
        maybe (return ()) unmask action)
  where
    lookupAction key rm@(Finalizers key' ref im) = case I.lookup key im of
        Nothing -> (rm, Nothing)
        Just action -> (Finalizers key' ref $ I.delete key im, Just action)


------------------------------------------------------------------------------
stateAlloc :: MonadST v i => v (Finalizers i) -> i ()
stateAlloc istate = atomicModifyRef' istate $ \(Finalizers key ref im) ->
    (Finalizers key (ref + 1) im, ())


------------------------------------------------------------------------------
stateCleanup :: (MonadST v i, MonadTry i) => v (Finalizers i) -> i ()
stateCleanup istate = mask_ $ do
    (ref, im) <- atomicModifyRef' istate $ \(Finalizers key ref im) -> do
        (Finalizers key (ref - 1) im, (ref - 1, im))
    when (ref == 0) $ do
        mapM_ mtry $ I.elems im
        writeRef istate $ undefined
