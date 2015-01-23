{-# LANGUAGE ExistentialQuantification #-}

module Control.SMealy where

import Prelude hiding ((.), id)
import Control.Category (Category(..))

data SMealy a b = forall x . SMealy x (a -> x -> (x,b))

instance Functor (SMealy a) where
    fmap f (SMealy begin step) =
        SMealy begin (\a -> (\(x,b) -> (x, f b)) . step a)
    {-# INLINABLE fmap #-}

instance Category SMealy where

    id = SMealy () (\a () -> ((),a))

    (.) (SMealy begin1 step1) (SMealy begin0 step0) =
      SMealy begin step
      where
        begin = (begin1,begin0)
        step a (x1,x0) = let (x0',b) = step0 a x0
                             (x1',c) = step1 b x1
                         in
                         ((x1',x0'),c)  
    {-# INLINABLE (.) #-}


