import Data.Char

doubleMe :: Int -> Int
doubleMe x = x + x

quadrupleMe :: Int -> Int
quadrupleMe x = doubleMe (doubleMe x)

qOne :: (Int -> a ) -> a
qOne f = f(10)

extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs) = if isDigit x then extractDigits xs else x:(extractDigits xs)

palindrome :: (Eq a) => [a] -> Bool
palindrome xs =  (xs == reverse xs)

squareRoot :: Float -> Float
squareRoot x = sqrt x

testlist =["madam", "adam", "otto", "elsa", "kajak", "tomas"]

test0 = map (+3)  [2,3,4,5,6]
testlcompr0 = [x+3| x<- [2,3,4,5,6]]

test1 = map palindrome testlist
testlcompr1 = [ palindrome (x) | x <- testlist]

test2 = filter palindrome testlist
testlcompr2 = [ x | x <- testlist, palindrome x]

square :: Int -> Int
square x = x * x

test3 = [(square x) + 3| x <-[1..500]]
myTest3 = map (\x -> square (x) + 3) [1..500]