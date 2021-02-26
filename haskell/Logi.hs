-- 2021.2.2
-- 一些不太成熟的尝试
-- 抽象和封装在改动实现时的确很有用
-- 不过代码本身是失败的。


{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Logi where

import Prelude hiding (fail,succ)
import Data.List
import Data.Monoid

fail :: [a]
fail = []

succ :: a -> [a]
succ x = [x]


data Expr
data Const
data Sym

data Term :: * where
    Var :: String -> Term
    Val :: String -> Term
--    Arrow :: Term -> Term -> Term
    Prod :: Term -> Term -> Term
    BOTTOM :: Term


instance Eq Term where
    (Var x) == (Var y) = x == y
    (Val x) == (Val y) = x == y
--    (Arrow x a) == (Arrow y b) = x == y && a == b
    (Prod x a) == (Prod y b) = x == y && a == b
    _ == _ = False

instance Show Term where
    show (Var x) = "?" ++ x
    show (Val x) = x
--    show (Arrow x xs) = "(" ++ show x ++ ") -> " ++ show xs
    show (Prod x xs) = show x ++ " . " ++ show xs

    


type Substitution = [(Term, Wrapper)]

type Goal = Substitution -> [Substitution]

data Wrapper = Inl Term | Inr Term deriving (Eq)

instance Show Wrapper where
    show (Inl x) = show x
    show (Inr x) = show x

injL :: String -> Wrapper
injL = Inl . Val

injR :: String -> Wrapper
injR = Inr . Var

walk :: Term -> Substitution -> Term
walk x@(Var _) s = case lookup x s of {
                   Nothing -> x;
                  (Just (Inl y@(Val _))) -> y;
                  (Just (Inr y@(Var _))) -> walk y s
                  }

walk x s = x

unifY :: Term -> Term -> Substitution -> [Substitution]
unifY (Val x) (Val y) s = if x == y then succ s else fail
unifY x@(Var _) (Val y) s = succ $ [(x, injL y)] ++ s
unifY (Val y) x@(Var _) s = succ $ [(x, injL y)] ++ s
unifY (Var x) (Var y) s = if x == y then succ s else succ $ [(Var x, injR y)] ++ s
-- unifY (Arrow x xs) (Arrow y ys) s = case  unify x y s of { [] -> fail; [s'] -> unify xs ys s' }
unifY (Prod x xs) (Prod y ys) s = case  unify x y s of { [] -> fail; [s'] -> unify xs ys s' }
unifY _ _ s = fail

unify x y s = unifY (walk x s) (walk y s) s

(===) :: Term -> Term -> Substitution -> [Substitution]
(===) = unify

any :: [Goal] -> Goal
any fs s = foldr (++) [] $ map ($ s) fs where
          

one :: [Goal] -> Goal
one [] s = []
one (f:fs) s = case f s of {
               [] -> one fs s;
               t@(_:_) -> t
               }

all :: [Goal] -> Goal
all fs s = go fs (succ s) where
     go [] ss = ss
     go (f:rest) ss = concatMap f ss

neg :: Goal -> Goal
neg g s = if g s == [] then succ s else fail


type State = Int

initST :: State
initST = 0

fresh :: State -> [String] -> ([Term],State)
fresh n l = foldr go ([],n) l where
      go str (acc,state) = ([Var $ str ++ show state] ++ acc, state + 1)


