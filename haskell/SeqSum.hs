-- maximum segment sum
-- 同构 的 课后习题
-- 很有优化空间，但是这是我(不完全独立)思考的结果，留念
-- (2020.12.8) 错误一处
-- 优化还得看TFWH

solve :: [Int] -> (Int,[Int])
solve [] = (0,[])
solve l = search $ map mkSum $ segments l where
    mkSum l = let s = sum l in s `seq` (s,l)
    search (x:xs) = helper x xs
    helper t@(n1,l1) [] = t
    helper t1@(n1,l1) (t2@(n2,l2):xs) | n1 < n2 = helper t2 xs
                                  | otherwise = helper t1 xs

segments l = snd $ foldr go ([[]],[[]]) l where
    go n (s,l) = (,) ([]:(update n s)) (update n s ++ l)
    update n l = map (n:) l

    