-- file: ch03/ListADT.hs
data List a = Cons a (List a)
   |Nil
   deriving (Show)
    
--data Tree a = Node a (Tree a) (Tree a)
--    |Empty
--    deriving (Show)
    
list = Cons 2 (Cons 1 Nil)

toList :: (List a) -> [a]
toList (Cons x xs) = x : (toList xs)
toList Nil = []
--toList (Cons x xs) = [x,(toList xs)]

data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Show)
    
l = Tree "leftChild" Nothing Nothing
r = Tree "rightChild" Nothing Nothing
p = Tree "parent" (Just l) (Just r)
