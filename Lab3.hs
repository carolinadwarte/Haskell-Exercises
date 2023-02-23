palindrome :: String -> String
palindrome xs = xs ++ reverse xs

euclidian ::(Floating a)=> (a, a, a) -> (a, a, a) -> Float
euclidian (x1,x2,x3) (y1,y2,y3) = sqrt((x1-y1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

safetail1:: Eq a => [a] -> [a]
safetail1 (x:xs) = if null[xs] then [] else xs

safetail2 :: Eq a => [a] -> [a]
safetail2 xs
  | xs == []  = []
  | otherwise = tail xs
  
safetail3:: Eq a => [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

--i) [x*x | x <- [0..499]]
--ii) [x | x <- [22..85], x `mod` 11 /= 0]
--iii) [x | x <- [17..1012], reverse (show x) == show x]

perfects :: Int -> [Int]
factorSum n = sum [x | x <- [1..n], n `mod` x == 0, x /= n]
perfects n = [x | x <- [1..n], factorSum x == x]