data Direction = North | East | South | West
         deriving (Show, Eq)
turnAround :: Direction -> Direction
turnAround North = South
turnAround South = North
turnAround West = East
turnAround East = West

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft West = South
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight West = North
turnRight East = South

safetailMaybe :: [a] -> Maybe [a]
safetailMaybe [] = Nothing
safetailMaybe (_:xs) = Just xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe x y = if x > length y then Nothing
else Just (take x y)

data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)

ex1 = Unary (Unary (Unary (Unary (Unary (Unary (Unary (Leaf 0) 1) 2) 3) 4) 5) 6) 7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Binary (Leaf 4) 5 (Leaf 6))

depth :: Btree a -> Int
depth (Leaf _ ) = 0
depth (Unary a _ ) = depth a + 1
depth (Binary a _ b) 
    | depth a > depth b = depth a + 1
    | otherwise = depth b + 1 

btreeToList :: Btree a -> [a]
btreeToList (Leaf x)       = [x]
btreeToList (Unary l x)    = btreeToList l ++ [x]
btreeToList (Binary l x r) = btreeToList l ++ x : btreeToList r

mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree f (Leaf x) = Leaf (f x)
mapBtree f (Unary l x) = Unary (mapBtree f l) (f x)
mapBtree f (Binary l x r) = Binary (mapBtree f l) (f x) (mapBtree f r)

--map (+3) (btreeToList ex1)
--[3,4,5,6,7,8,9,10]
--btreeToList (mapBtree (+3) ex1)
--[3,4,5,6,7,8,9,10]