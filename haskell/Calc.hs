module Calc where

import Data.Monoid

data FData = FInt Int | FChar Char | FDouble Double deriving (Show, Eq)

data FStack = Result [FData] [FData]
            | FErr String deriving (Show,Eq)

type FWord = (FStack -> FStack) -> (FStack -> FStack)

instance Monoid FWord where
    mempty = id
    mappend = f . g


evalW :: FWord -> FStack
evalW w = w id ([],[])

evalF :: [FWord] -> FStack
evalF = evalW . mconcat



 
