-- file: ch04/ch04.exercises.hs
import System.Environment (getArgs)
import Data.Char (digitToInt)

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
                  where helper acc y    |acc > ((acc * 10) + (digitToInt y)) = error "Your number is too big for an Int"
                                        |otherwise = (acc * 10) + (digitToInt y)
          
          
-- type ErrorMessage = String
-- asInt_either :: String -> Either ErrorMessage Int 
-- asInt_either "" = Left "This String is empty."
-- asInt_either ('-':xs)   |xs == [] = Left "Not a number."
--                        |otherwise = -1 * (asInt_fold xs)
-- asInt_either (x:xs) |elem '.' (x:xs) = Left "Decimals cannot be parsed by this function."
                    -- |otherwise = (foldl helper 0 (x:xs))
                    -- where helper acc y  |acc > ((acc * 10) + (digitToInt y)) = Left "Your number is too big for an Int"
                                        -- |otherwise = (acc * 10) + (digitToInt y)
          
          
          
          
          
          
          
          
          
          
          
          
