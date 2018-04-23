-- file: ch04/Sum.hs
mySum xs = helper 0 xs
    where helper sum (x:xs) = helper (sum + x) xs
          helper sum _      = sum
          
foldlSum xs = foldl (+) 0 xs
