-- file: ch03/Intersperse.hs
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse separator (x:xs)
    | null xs = x
    | otherwise = x ++ [separator] ++ (myIntersperse separator xs)