mss :: [Int] -> Int
mss l = fst $ foldr go (0,0) l where
    go x (val, seed) = let val' = maximum [val, x, x + seed] in (val', max x (seed + x))

main = print $ mss [2, -1, 3, 4]

-- output : 8

-- waiting for monadic version
