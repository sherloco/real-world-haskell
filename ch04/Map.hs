-- file ch04/Map.hs
square :: [Double] -> [Double]
square [] = []
square (x:xs) = (x*x):(square xs)
