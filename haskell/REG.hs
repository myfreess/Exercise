module REG where

data RegexP = Lit Char 
            | Empty
            | Concat RegexP RegexP
            | Choose RegexP RegexP
            | Repeat RegexP deriving (Eq,Show)

type Tokens = [Char]

zerok :: Tokens -> Bool
zerok [] = True
zerok _ = False

matchRegexP :: RegexP -> Tokens -> (Tokens -> Bool) -> Bool
matchRegexP (Lit c) s k | s == [] = False
                        | otherwise = (head s) == c && k (tail s)
matchRegexP Empty s k = k s
matchRegexP (Concat r1 r2) s k = matchRegexP r1 s (\s'-> matchRegexP r2 s' k)
matchRegexP (Choose r1 r2) s k = matchRegexP r1 s k || matchRegexP r2 s k -- lazyness
matchRegexP (Repeat Empty) s k = k s
matchRegexP (Repeat r) s k = matchRegexP r s (\s'-> matchRegexP (Repeat r) s' k) || k s

match :: RegexP -> Tokens -> Bool
match r s = matchRegexP r s zerok
