-- file: ch03/Nullable.hs
data MyMaybe a = MyJust a
    | MyNothing
    deriving (Show)

someBool = Just True

someString = Just "something"

wrapped = Just(Just "wrapped")

myWrapped = MyJust(MyJust "wrapped")
