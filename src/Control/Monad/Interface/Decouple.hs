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
instance (MonadLayer m, MonadDecouple i (Inner m)) => MonadDecouple i m where
    decouple = layer . liftM (fmap layer) . decouple
    {-# INLINE decouple #-}