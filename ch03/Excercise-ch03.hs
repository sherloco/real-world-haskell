-- file: ch03/Excercise-ch03.hs
import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

mean [] = error "Division by zero."
mean (x:xs) = (fromIntegral (sum (x:xs))) / (fromIntegral (length (x:xs)))

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = (x:xs)++(myReverse (x:xs))
          
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) | odd (length (x:xs)) = False
isPalindrome (x:xs) = let sizeOfHalfList = round (fromIntegral (length (x:xs))/2)
                          firstHalf = take sizeOfHalfList (x:xs)
                          secondHalfReversed = myReverse (drop sizeOfHalfList (x:xs))
                          equal [] [] = True
                          equal (x:xs) (y:ys) = (x == y) && (equal xs ys)
                      in equal firstHalf secondHalfReversed
                               
sortListsByLength lst = sortBy compareLength lst
    where compareLength lst1 lst2
            | (length lst1) > (length lst2) = GT
            | (length lst1) < (length lst2) = LT
            | otherwise = EQ
            
data Tree a = Node a (Tree a) (Tree a)
    |Empty
    deriving (Show)

treeHeight :: Tree a -> Int
treeHeight (Empty) = 0
treeHeight (Node _ b c) = 1 + max (treeHeight b) (treeHeight c)

data Direction = LeftTurn
    |RightTurn
    |Straight
    deriving (Show,Eq)

calculateDirection :: (RealFrac a) => (a,a) -> (a,a) -> (a,a) -> Direction
calculateDirection p1 p2 p3 =
    let ccw = (((fst p2) - (fst p1))*((snd p3) - (snd p1))) - (((snd p2) - (snd p1))*((fst p3) - (fst p1)))
    in case ccw of ccw
                    |ccw > 0 -> LeftTurn
                    |ccw < 0 -> RightTurn
                    |otherwise -> Straight


calculateDirections :: (RealFrac a) => [(a,a)] -> [Direction]
calculateDirections (x:xs) 
    |(length xs) >= 2 = (calculateDirection x (head xs) (head (tail xs))):(calculateDirections xs)
    |otherwise = []

--grahamScan :: (RealFrac a) => [(a,a)] -> [(a,a)]
grahamScan lst = 
    let n = length lst
        compareYValue p1 p2
            |(snd p1)<(snd p2) = LT
            |(snd p1)>(snd p2) = GT
            |(fst p1)<(fst p2) = LT
            |(fst p1)<(fst p2) = GT
            |otherwise = EQ
        (px,py) = minimumBy compareYValue lst
        compareAngleWithXAxis (p1x,p1y) (p2x,p2y)
            |(calculateDirection (0,0) (p1x-px,p1y-py) (p2x-px,p2y-py)) == LeftTurn = LT
            |(calculateDirection (0,0) (p1x-px,p1y-py) (p2x-px,p2y-py)) == RightTurn = GT
            |otherwise = EQ
        sortedPoints = sortBy compareAngleWithXAxis lst
        helper (x:(y:(z:zs)))
                |(calculateDirection x y z) == RightTurn = (helper (x:(z:zs)))
                |otherwise = y:(helper (y:(z:zs)))
        helper xs = xs
        rmdups [] = []
        rmdups (x:[]) = x:[]
        rmdups (x:(y:ys))
            |(fst x) == (fst y) && (snd x) == (snd y) = rmdups (y:ys)
            |otherwise = x:(rmdups (y:ys))
    in  rmdups (helper (sortedPoints ++ [(px,py)]))
                            
                            

-- grahamScan [(1,1), (2,5), (3,3), (5,3), (3,2), (2,2)]
-- [(2.0,5.0),(5.0,3.0),(1.0,1.0)]