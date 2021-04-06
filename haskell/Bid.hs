{-# LANGUAGE ScopedTypeVariables #-}

module Bid where

inverseMap :: forall e s . (Bounded e, Enum e, Eq e, Ord s) => (e -> s) -> s -> Maybe e -- 懒了
inverseMap f source = go minBound where
    go :: e -> Maybe e
    go dest | dest == maxBound = if f dest  == source then Just dest else Nothing
            | otherwise = if f dest == source then Just dest else go (succ dest)


