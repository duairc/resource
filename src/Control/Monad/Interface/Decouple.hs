{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Decouple
    ( MonadDecouple (decouple)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer)
                     , MonadLift
                     )


-- couple --------------------------------------------------------------------
import           Control.Monad.Trans.Couple (CoupleT)


------------------------------------------------------------------------------
class (Monad i, Monad m, MonadLift i m) => MonadDecouple i m where
    decouple :: CoupleT i a -> m (a, m ())


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayer m, MonadDecouple i (Inner m)) =>
#else
instance (MonadLayer m, MonadDecouple i (Inner m), MonadLift i m) =>
#endif
    MonadDecouple i m
  where
    decouple = layer . liftM (fmap layer) . decouple
    {-# INLINE decouple #-}
