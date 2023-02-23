doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x + x + y + y

doubleUs2 :: Int -> Int -> Int
doubleUs2 x y = doubleMe x + doubleMe y

quadrupleMe :: Int -> Int
quadrupleMe x = doubleMe (doubleMe x)

third:: [Int] -> Int
third x = x !! 2

last2:: [Int] -> Int
last2 x = head (reverse x)

init2 :: [Int] -> [Int]
init2 x = reverse (tail (reverse x))

last3 :: [Int] -> Int
last3 x = x !! (length x-1)

init3 :: [Int] -> [Int]
init3 x = reverse (drop 1 (reverse x))