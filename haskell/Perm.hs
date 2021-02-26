module Perm where

import Data.Monoid

fromJust ~(Just x) = x

type FreqTable = [(Char, Int)]

update :: Char -> FreqTable -> FreqTable
update c [] = [(c,1)]
update c (p@(ch, count) : xs) = if ch == c then (ch, count + 1) : xs else p : update c xs
-- 每次会把新字符放到表底部，算了，就这样吧，不用Map了, 也不写尾递归了

consume :: Char -> FreqTable -> FreqTable -> Maybe FreqTable
consume c (p@(ch,count):xs) acc | ch == c = if count == 1 then Just $ xs ++ acc else Just $ [(ch,count - 1)] ++ xs ++ acc
                                | otherwise = consume c xs (p:acc)

consume c [] acc = Nothing


countCh :: String -> FreqTable
countCh = foldr update []

dfs :: (String -> String) -> FreqTable -> Maybe [String]
dfs f [] = Just $ [f ""]
dfs f t = mconcat $ do { x <- t; let {r = consume (fst x) t []} in if r == Nothing then  [Nothing] else [dfs (((fst x) :) . f) $ fromJust r]}




permutations :: String -> [String]
permutations "" = [""]
permutations s = fromJust $ dfs id (countCh s)



