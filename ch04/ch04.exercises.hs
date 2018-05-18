-- file: ch04/ch04.exercises.hs
import System.Environment (getArgs)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Either
import Control.Exception
import System.IO.Unsafe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeFunction :: ([a] -> b) -> [a] -> Maybe b
safeFunction _ [] = Nothing
safeFunction fct xs = Just (fct xs)

safeTail :: [a] -> Maybe [a]
safeTail xs = safeFunction tail xs

safeLast :: [a] -> Maybe a
safeLast xs = safeFunction last xs

safeInit :: [a] -> Maybe [a]
safeInit xs = safeFunction init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred (x:xs)
    |not (pred x)   = (takeWhile (not . pred) (x:xs)):(splitWith pred (dropWhile pred xs))
    |otherwise      = splitWith pred (dropWhile pred (x:xs))
splitWith _ [] = []

printFirstWordOfEachLine :: String -> String
printFirstWordOfEachLine s = unlines (map (head . words) (lines s))

transpose :: String -> String
transpose s = concat (zipWith zipper (head (lines s)) (last (lines s)))
    where zipper x y = x:y:'\n':[]



interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
                
          -- replace "id" with the name of our function below
          myFunction = printFirstWordOfEachLine


asInt_fold :: String -> Int
asInt_fold "" = error "This String is empty."
asInt_fold ('-':xs) |xs == [] = error "Not a number."
                    |otherwise = -1 * (asInt_fold xs)
asInt_fold (x:xs) |elem '.' (x:xs) = error "Decimals cannot be parsewd by this function."
                  |otherwise = foldl helper 0 (x:xs)
                  where helper acc y    |checkIfOverflow acc (digitToInt y) = error "Your number is too big for an Int"
                                        |otherwise = (acc * 10) + (digitToInt y)
                        checkIfOverflow :: Int -> Int -> Bool
                        checkIfOverflow acc y = (checkIfMultOverflow acc) || (checkIfAddOverflow acc y)
                        checkIfMultOverflow acc = ((fromIntegral (acc * 10)) / fromIntegral 10) /= fromIntegral acc 
                        checkIfAddOverflow acc y = (acc * 10) > ((acc*10)+y)



 
-- I guess this not the solution the book demands. I postpone it until I now more about monads and error handling. 
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int 
asInt_either "" = Left "This String is empty."
asInt_either ('-':xs)   |xs == [] = Left "Not a number."
                        |otherwise = let res = (asInt_either xs)
                                     in if (isLeft res)
                                        then res
                                        else Right ( -1 * (fromRight 0 res))
asInt_either (x:xs)     |elem '.' (x:xs) = Left "Decimals cannot be parsed by this function."
                        |otherwise = foldl helper (Right 0) (x:xs)
                        where helper acc y  |isLeft acc = acc
                                            |not (isDigit y) = Left ("non-digit '" ++ [y] ++ "'")
                                            |checkIfOverflow (fromRight 0 acc) (digitToInt y) = Left "Your number is too big for an Int"
                                            |otherwise = Right (((fromRight 0 acc) * 10) + (digitToInt y))
                              checkIfOverflow :: Int -> Int -> Bool
                              checkIfOverflow acc y = (checkIfMultOverflow acc) || (checkIfAddOverflow acc y)
                              checkIfMultOverflow acc = ((fromIntegral (acc * 10)) / fromIntegral 10) /= fromIntegral acc 
                              checkIfAddOverflow acc y = (acc * 10) > ((acc*10)+y)

                            
myConcat :: [[x]] -> [x]
myConcat xs = foldr (++) [] xs
          
takeWhileRec :: (a -> Bool) -> [a] -> [a]
takeWhileRec _ [] = []
takeWhileRec pred (x:xs) |not (pred x) = []
                         |otherwise = x:(takeWhileRec pred xs)
                         
takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr pred xs = foldr step [] xs
                            where step x ys |pred x = x : ys
                                            |otherwise = []

-- This implementation gives a different results than the standard implementation in some cases:
-- *Main Data.List> myGroupBy (\x y -> (x*y `mod` 3) == 0) [1,2,3,4,5,6,7,8,9] 
-- [[1],[2,3,4],[5,6,7],[8,9]]
-- *Main Data.List> groupBy (\x y -> (x*y `mod` 3) == 0) [1,2,3,4,5,6,7,8,9] 
-- [[1],[2,3],[4],[5,6],[7],[8,9]]
-- The definition documentaions says "The function is assumed to define a total ordering" which is not the case here.
-- Found out the Prelude version does check against the first element of each sublist while I check angainst the neighboring element.
-- However, this makes no difference if a total ordering is used.                              
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy pred xs = foldr step [[]] xs
                        where step x [[]] = [[x]]
                              step x (y:ys) |pred x (head y) = (x:y):ys
                                            |otherwise = [x]:y:ys


                                            
-- any can be implemented using both foldl or foldr. However, both variant are not 
-- very efficient since they have to go through the hole list instead of stoppping 
-- in case an element that satisfies the pred.
anyFoldl :: (a -> Bool) -> [a] -> Bool 
anyFoldl pred xs = foldl helper False xs
                    where helper b x = b || pred x

anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr pred xs = foldr helper False xs
                    where helper x b = b || pred x

-- Here is a more efficient variant that works on infinte lists, too (At least when there is an element that satifies the pred).
-- This is because of the lazy evaluation and || returning true without checking the second argument.
-- Therefore, foldr is more appropriate for this task.
anyFoldr2 :: (a -> Bool) -> [a] -> Bool
anyFoldr2 pred xs = foldr helper False xs
                    where helper x b = pred x || b


-- Foldr looks way more suitable here.
cycleFoldr :: [a] -> [a]
cycleFoldr xs = foldr (:) (cycleFoldr xs) xs       

-- But I like the most this very readble and very short form
cycleNoFold xs = xs ++ (cycleNoFold xs)
          
wordsFoldr :: String -> [String]
wordsFoldr xs = filter (\l -> (not (null l))) (foldr step [[]] xs)
                            where step x (y:ys) |isSpace x = []:y:ys
                                                |otherwise = (x:y):ys

                                                
wordsFoldl :: String -> [String]
wordsFoldl xs = snd (foldl step ("",[]) (xs++"\n"))
                        where   step (cur,acc) x    |isSpace x && null cur = ("", acc)
                                                    |isSpace x = ("",acc ++ [cur])
                                                    |otherwise = (cur ++ [x], acc)    
                          

--wordsFoldl xs = foldl step "" xs
--                    where step x (y:ys) = x ++ y ++ "\n"





