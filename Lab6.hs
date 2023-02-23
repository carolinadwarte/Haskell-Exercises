qOne :: IO ()
qOne = do {
            putStr "Please enter a large number:" ;
            x <- getLine ;
            putStr x ;
            putChar '\n' }
 
            
qOne2 :: IO ()
qOne2 = do {
   putStrLn "Please enter a large number:" ;
   x <- getLine ;
   putStr x ;
   if read x >= 100
   then putStr "That is a large number." 
   else do {
   putStr "That number isn't large." ; 
   qOne2 ; 
   }
   }
   

printStars :: Int -> IO ()
printStars x = do {
            let y = replicate x '*' in
            putStrLn y ;
            }
            

printStars2 :: Int -> IO ()
printStars2 0 = putStr []
printStars2 x = do {
    printStars2 (x - 1) ;
    printStars x ;
}
