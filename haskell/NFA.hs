{-# LANGUAGE ScopedTypeVariables #-}

module NFA where


import Data.List

type State = Int
type Token = Char
type States = [State]

data FARule = FARule State Token State deriving (Eq)

instance Show FARule where
    show (FARule s ch s') | ch == '~' = show s ++ " ==~==> " ++ show s'
                          | otherwise = show s ++ " ==#[" ++ [ch] ++ "]#==> " ++ show s'

type NFARules = [FARule]

type NFA = (States,States,NFARules)

elimRules :: NFA -> NFARules
elimRules (_,_,x) = x

nextStates :: NFARules -> States -> Char -> States
nextStates rs ss ch = nub $ foldr go [] rs where
    go :: FARule -> States -> States
    go (FARule s c s') pack | s `elem` ss && c == ch = (s':pack)
                            | otherwise = pack


freeSign :: Token
freeSign = '~' -- representing free move

isSubsetOf :: forall a . Eq a => [a] -> [a] -> Bool
isSubsetOf l r = foldr go True l where
     go :: a -> Bool -> Bool
     go e b = e `elem` r && b

freeMove :: NFARules -> States -> States
freeMove rs ss = let { ss' = nextStates rs ss '~' } in
        if ss' `isSubsetOf` ss -- fix point!
        then ss else freeMove rs (nub (ss' ++ ss))


(<==<) :: NFA -> [Token] -> Bool
(s,target,rules) <==< [] = s == target
(s,target,rules) <==< (x:xs) = (newS,target,rules) <==< xs where
     newS = nextStates rules (freeMove rules s) x



