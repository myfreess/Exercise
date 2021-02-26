import Prelude hiding (init)
import Data.Coerce

newtype Brc = Brc Int deriving (Show, Eq)

leftP = Brc 1
rightP = Brc $ 0 - 1

toInts :: [Brc] -> [Int]
toInts = coerce

notVaildfor x n = let {r = sum . toInts $ x} in r < 0 || r > n + 1 

-- n 为偶数
init :: Int -> Int -> [[Brc]]
init 0 lim = [[]]
-- sum . toInts x > lim + 1 时同样可过滤
init n lim = foldr go [] (init (n - 1) lim) where
    go x acc | x `notVaildfor` lim = acc
             | otherwise = [(leftP:x), (rightP:x)] ++ acc

toBracket :: [Brc] -> String
toBracket l = map trans l where
    trans x | x == leftP = ')'
            | x == rightP = '('
            | otherwise = undefined


balancedParens :: Int -> [String]
balancedParens n | n < 0 = undefined
                 | otherwise = map toBracket $ filter (\x -> (sum . toInts) x == 0) $ init (n * 2) n

main = print $ balancedParens 4

