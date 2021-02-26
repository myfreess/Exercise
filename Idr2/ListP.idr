module ListP



app_commutes : (x : List a) -> (y : List a) -> (z : List a) -> x ++ y ++ z = x ++ (y ++ z)
app_commutes [] y z = Refl
app_commutes (x :: xs) y z = cong (x ::) (app_commutes xs y z) 