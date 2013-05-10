{-# LANGUAGE Unsafe #-}

module Control.Monad.Trans.Couple.Unsafe
    ( unsafeDecouple
    )
where


-- layers-bracket ------------------------------------------------------------
import           Control.Monad.Trans.Couple.Internal (CoupleT (CoupleT))


------------------------------------------------------------------------------
unsafeDecouple :: Monad m => CoupleT m a -> m (a, m ())
unsafeDecouple (CoupleT m) = m
{-# INLINE unsafeDecouple #-}