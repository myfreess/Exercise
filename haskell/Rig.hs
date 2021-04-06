{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}


module Rig where

data ZERO
data POSITIVE
data NEGATIVE

newtype IntF a = IntF Int

zero :: IntF ZERO
zero = IntF 0

class RigR a b c | a b -> c where
      mul :: IntF a -> IntF b -> IntF c



instance RigR ZERO a ZERO where
      mul _ _ = zero

instance RigR a ZERO ZERO where
      mul _ _ = zero

instance RigR NEGATIVE POSITIVE NEGATIVE where
      mul (IntF x) (IntF y) = IntF (x * y)

instance RigR POSITIVE NEGATIVE NEGATIVE where
      mul (IntF x) (IntF y) = IntF (x * y)

instance RigR NEGATIVE NEGATIVE POSITIVE where
      mul (IntF x) (IntF y) = IntF (x * y)

instance RigR POSITIVE POSITIVE POSITIVE where
      mul (IntF x) (IntF y) = IntF (x * y)








