{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Coloring where

-- Product
data Assoc x y

-- lists
data Nil
data Cons x y

data True
data False


data Z
data S a

type S1 = S Z
type S2 = S S1
type S3 = S S2
type S4 = S S3
type S5 = S S4

data CZ
data CS a

type Yellow = CZ
type Red = CS Yellow
type Green = CS Red
type Blue = CS Green

type Graph = Cons (Assoc S1 S2) (Cons (Assoc S1 S3) (Cons (Assoc S1 S4) (Cons (Assoc S1 S5) (Cons (Assoc S2 S3) (Cons (Assoc S2 S4) (Cons (Assoc S3 S4) (Cons (Assoc S4 S5) Nil)))))))

type Config = Cons Red (Cons Green (Cons Blue (Cons Yellow Nil)))

-- Constraint definition

class And b1 b2 b | b1 b2 -> b
instance And True  True  True
instance And True  False False
instance And False True  False
instance And False False False

class Or b1 b2 b | b1 b2 -> b
instance Or True  True  True
instance Or True  False True
instance Or False True  True
instance Or False False False

class Eql x y b | x y -> b

instance Eql Z Z True
instance Eql Z (S a) False
instance Eql (S a) Z False
instance (Eql x y b) => Eql (S x) (S y) b
instance Eql True True True
instance Eql True False False
instance Eql False True False
instance Eql False False True
instance Eql CZ CZ True
instance Eql CZ (CS a) False
instance Eql (CS a) CZ False
instance (Eql x y b) => Eql (CS x) (CS y) b
instance (Eql x1 x2 b1, Eql y1 y2 b2, And b1 b2 r) => Eql (Assoc x1 y1) (Assoc x2 y2) r
instance Eql Nil Nil True
instance Eql Nil (Cons x xs) False
instance Eql (Cons x xs) Nil False
instance (Eql x y b1, Eql xs ys b2, And b1 b2 b) => Eql (Cons x xs) (Cons y ys) b



class MemberQ e l b | e l -> b

instance MemberQ e Nil False
instance (Eql e x b1, MemberQ e xs b2, Or b1 b2 b) => MemberQ e (Cons x xs) b



class Adjacent x y l r | x y l -> r

instance (MemberQ (Assoc x y) l b1, MemberQ (Assoc y x) l b2, Or b1 b2 r) => Adjacent x y l r


