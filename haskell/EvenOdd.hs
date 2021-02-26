data Nat = Z | S Nat

data M a = M (M a -> a) (M a -> a)

fstM , sndM :: M a -> M a -> a
fstM (M f _) = f
sndM (M _ f) = f

isEven_ :: M (Nat -> Bool) -> Nat -> Bool
isEven_ _ Z = True
isEven_ (M f g) (S n) = g (M f g) n

isOdd_ :: M (Nat -> Bool) -> Nat -> Bool
isOdd_ _ Z = False
isOdd_ (M f g) (S n) = f (M f g) n

packOfFunction :: M (Nat -> Bool)
packOfFunction = M isEven_ isOdd_

isEven, isOdd :: Nat -> Bool
isEven = isEven_ packOfFunction
isOdd = isOdd_ packOfFunction

intToNat :: Int -> Nat
intToNat n | n < 0 = intToNat (- n)
        | n == 0 = Z
        | otherwise = S $ intToNat (n - 1)



isEvenTestWith :: Int -> IO ()
isEvenTestWith n = putStrLn $ "| " ++ show n ++ " => Even Number | is : " ++ (show $ isEven $ intToNat n)

main = mapM_ isEvenTestWith [0..9]
