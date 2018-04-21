--file : ch04/myListFunctions.hs
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs)   = (1 + (myLength xs))

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myHead :: [a] -> a
myHead (x:xs) = x
myHead [] = error "empty list"

myTail :: [a] -> [a]
myTail (x:xs) = xs
myTail [] = error "empty list"

myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs
myLast [] = error "empty list"

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)

-- ++
myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] xs = xs
myPlusPlus xs [] = xs
myPlusPlus xs ys = myPlusPlus (myInit xs) ((myLast xs):ys)

myConcat :: [[a]] -> [a]
myConcat [xs] = xs
myConcat (x:xs) = myPlusPlus x (myConcat xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myPlusPlus (myReverse xs) [x]

myAnd :: [Bool] -> Bool
myAnd (False:xs) = False
myAnd (True:xs) = myAnd xs
myAnd [] = True

myOr :: [Bool] -> Bool
myOr (True:xs) = True
myOr (False:xs) = myOr xs
myOr [] = False

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred (x:xs) = if (pred x)
                    then myAll pred xs
                    else False
myAll _ [] = True

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred (x:xs)
    |pred x = True
    |otherwise = myAny pred xs
myAny _ [] = False

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt n xs = (take n xs, drop n xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred (x:xs)
    |pred x     = x:(myTakeWhile pred xs)
    |otherwise  = []
myTakeWhile pred [] = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile pred (x:xs)
    |pred x     = myDropWhile pred xs
    |otherwise  = (x:xs)
myDropWhile pred [] = []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred (x:xs)
    |pred x = x:(myFilter pred xs)
    |otherwise = myFilter pred xs
myFilter _ [] = []


myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
--a `myIsPrefixOf` b = myIsPrefixOf a b
myIsPrefixOf (x:xs) (y:ys)
    |not (x == y) = False
    |otherwise = myIsPrefixOf xs ys
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False

myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf xs (y:ys)
    |myIsPrefixOf xs (y:ys) = True
    |otherwise              = myIsInfixOf xs ys
myIsInfixOf _ [] = False

myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf xs ys = myIsPrefixOf (myReverse xs) (myReverse ys)

myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
myZip _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op (x:xs) (y:ys) = (op x y):(myZipWith op xs ys)
myZipWith _ _ _ = []















