{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}
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
                     (
#if !MIN_VERSION_base(4, 8, 0)
                       Applicative (pure, (<*>))
                     ,
#endif
                       Alternative (empty, (<|>))
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
import           Control.Monad.IO.Class (MonadIO (liftIO))
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as F
#endif
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip (mzip, mzipWith, munzip))
#endif
import           Data.Functor.Identity (Identity (Identity))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Word (Word)
#endif


-- containers ----------------------------------------------------------------
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans, lift
                     , MonadTransControl, LayerResult, LayerState
                     , suspend, resume, capture, extract, control
                     , MInvariant, hoistiso
                     , MFunctor, hoist
                     , MonadInner, liftI
                     )
import           Monad.Fork (MonadFork, fork, forkOn)
import           Monad.Mask (MonadMask, mask, mask_)
import           Monad.ST (MonadST, atomicModifyRef', newRef, writeRef)
import           Monad.Try (MonadTry, mtry, onException)


-- resource ------------------------------------------------------------------
import           Monad.Safe.Internal
                     ( MonadSafe (register')
                     , ReleaseKey (ReleaseKey)
                     )
import           Data.Resource.Internal
                     ( Finalizers (Finalizers, onError, onSuccess)
                     )


------------------------------------------------------------------------------
data ReleaseMap i = ReleaseMap !Int !Word !(IntMap (Finalizers i))


------------------------------------------------------------------------------
newtype SafeT v i m a = SafeT (v (ReleaseMap i) -> m a)


------------------------------------------------------------------------------
instance MonadTrans (SafeT v i) where
    lift = SafeT . const


------------------------------------------------------------------------------
instance MonadTransControl (SafeT v i) where
    suspend (SafeT f) istate = liftM (\a -> (Identity a, istate)) (f istate)
    resume (Identity a, _) = return a
    capture = SafeT $ \istate -> return istate
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
type instance LayerResult (SafeT v i) = Identity
type instance LayerState (SafeT v i) = v (ReleaseMap i)


------------------------------------------------------------------------------
instance MInvariant (SafeT v i) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MFunctor (SafeT v i) where
    hoist f (SafeT m) = SafeT $ f . m


------------------------------------------------------------------------------
instance Monad m => Functor (SafeT v i m) where
    fmap = liftM


------------------------------------------------------------------------------
instance Monad m => Applicative (SafeT v i m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (SafeT v i m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
instance Monad m => Monad (SafeT v i m) where
    return = lift . return
    SafeT m >>= f = SafeT $ \r -> m r >>= \a -> let SafeT m' = f a in m' r
    fail = lift . fail


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (SafeT v i m) where
    mzero = lift mzero
    mplus a b = control (\run -> mplus (run a) (run b))


------------------------------------------------------------------------------
instance MonadFix m => MonadFix (SafeT v i m) where
    mfix f = control (\run -> mfix (\a -> run (resume a >>= f)))


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance MonadFail m => MonadFail (SafeT v i m) where
    fail = lift . F.fail


#endif
#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance MonadZip m => MonadZip (SafeT v i m) where
    mzipWith f = liftM2 f
    mzip = liftM2 (,)
    munzip m = (liftM fst m, liftM snd m)


#endif
------------------------------------------------------------------------------
instance MonadIO m => MonadIO (SafeT v i m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
instance 
    ( MonadFork m
    , MonadInner i m
    , MonadST v i
    , MonadTry i
    , MonadTry m
    )
  =>
    MonadFork (SafeT v i m)
  where
    fork (SafeT f) = SafeT $ \istate -> mask $ \restore -> do
        liftI $ stateAlloc istate
        fork $ do
            restore (f istate) `onException` liftI (stateCleanup False istate)
            liftI (stateCleanup True istate)

    forkOn n (SafeT f) = SafeT $ \istate -> mask $ \restore -> do
        liftI $ stateAlloc istate
        forkOn n $ do
            restore (f istate) `onException` liftI (stateCleanup False istate)
            liftI (stateCleanup True istate)


------------------------------------------------------------------------------
instance (MonadST v i, MonadInner i m, MonadMask i) =>
    MonadSafe i (SafeT v i m)
  where
    register' fin = SafeT $ \istate -> liftI $ register istate fin
    {-# INLINE register' #-}


------------------------------------------------------------------------------
runSafeT :: (MonadST v i, MonadInner i m, MonadTry i, MonadTry m)
    => SafeT v i m a
    -> m a
runSafeT (SafeT f :: SafeT v i m a) = do
    istate <- liftI $ (newRef $ ReleaseMap 0 0 I.empty :: i (v (ReleaseMap i)))
    mask $ \restore -> do
        liftI $ stateAlloc istate
        a <- restore (f istate) `onException` liftI (stateCleanup False istate)
        liftI $ stateCleanup True istate
        return a


------------------------------------------------------------------------------
register :: (MonadST v i, MonadMask i)
    => v (ReleaseMap i)
    -> Finalizers i
    -> i (ReleaseKey i)
register istate fin = atomicModifyRef' istate $ \(ReleaseMap k ref im) ->
    ( ReleaseMap (k + 1) ref (I.insert k fin im)
    , ReleaseKey $ atomicModifyRef' istate $ \rm@(ReleaseMap k' ref' im') ->
        case I.lookup k im' of
            Nothing -> (rm, Finalizers (return ()) (return ()))
            Just fin' -> (ReleaseMap k' ref' $ I.delete k im', fin'))


------------------------------------------------------------------------------
stateAlloc :: MonadST v i => v (ReleaseMap i) -> i ()
stateAlloc istate = atomicModifyRef' istate $ \(ReleaseMap k ref im) ->
    (ReleaseMap k (ref + 1) im, ())


------------------------------------------------------------------------------
stateCleanup :: (MonadST v i, MonadTry i) => Bool -> v (ReleaseMap i) -> i ()
stateCleanup success istate = mask_ $ do
    (ref, im) <- atomicModifyRef' istate $ \(ReleaseMap k ref im) -> do
        (ReleaseMap k (ref - 1) im, (ref - 1, im))
    when (ref == 0) $ do
        mapM_ (mtry . if success then onSuccess else onError) $ I.elems im
        writeRef istate $ undefined
