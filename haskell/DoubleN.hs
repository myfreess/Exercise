module Truth where

type Cont r a = (a -> r) -> r

data Void
-- Void实际上是一个无值类型
-- 在CH同构下，所有的无值类型都被当做False，即假命题
-- 函数类型，则被当作证明转化的桥梁
-- 所以，当a为一个有值类型时，a -> Void ≈ Void
-- 因为以真命题为条件是没得办法证明一个假命题的
-- 但是 forall a . Void -> a 都是正确命题
-- 因为从谎言(假命题) 什么都能推出来
-- (Cont Void) a 实际上是对a代表的命题双重否定
-- 故有人戏言 CPS其实就是Double Negation罢了!
-- 从实际来看，Cont r a描述了一个函数，它寄存了一个类型为a的值
-- 若a没有值，想构造出Cont r a的值自然是天方夜谭
-- 若a有值，尽管它的Continuation不存在，可只不过函数调用不了罢了，定义是可以写出来的啊! 

endness :: Cont Void Int
endness = \k -> k 42

-- 此有一例，送给人类的好朋友马文 