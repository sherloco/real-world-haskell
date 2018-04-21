-- file: ch04/ch04.exercises.hs
import System.Environment (getArgs)

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

