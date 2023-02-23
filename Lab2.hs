isEqual :: [Char] -> [Char] -> Bool
isEqual xs ys = if xs==ys
                then True
                else False

isEqual2 :: [Char] -> [Char] -> Int -> Bool
isEqual2 xs ys n = if xs!!n==ys!!n
                then True
                else False
                
third :: (a,b,c) -> c
third (x,y,z) = z

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

addsquares :: (Num a) => (a, a) -> a
addsquares  (x, y)   = x * x + y * y

ordered :: (Ord a) => a -> a -> a -> Bool
ordered x y z  = x<=y && y<=z

palindrome :: (Eq a) => [a] -> Bool
palindrome xs =  (xs == reverse xs)

--Challenge Task 

isEqual3 :: (Eq a) => [a] -> [a] -> Bool
isEqual3 xs ys = if xs==ys
                then True
                else False

isEqual4 :: (Eq a) => Int -> [a] -> [a] -> Bool
isEqual4 n xs ys = if n < length xs && n < length ys
                   then xs!!n==ys!!n
                   else False
                   
--                   
Prelude> :type ("a","b","c")
("a","b","c") :: ([Char], [Char], [Char])

Prelude> :type ["a","b","c"]
["a","b","c"] :: [[Char]]

Prelude> :type [("1", False), ("0",True)]
[("1", False), ("0",True)] :: [([Char], Bool)]

Prelude> :type ([True,False], ["0","1"])
([True,False], ["0","1"]) :: ([Bool], [[Char]])

Prelude> :cd CS-205
*Main> isEqual "abc" "abc"
True
*Main> isEqual "abc" "def"
False   
--                