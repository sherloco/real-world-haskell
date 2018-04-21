-- file: ch03/BadTree.hs
data Node a = Node a (Maybe (Node a)) (Maybe (Node a))
    deriving (Show)

nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing