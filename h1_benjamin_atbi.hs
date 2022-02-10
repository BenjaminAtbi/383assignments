
seqMultiple :: Int -> Int -> [Bool]
seqMultiple 0 _ = []
seqMultiple x 0 = replicate x False
seqMultiple x y = [ mod n y == 0 | n <- [1..x] ]

fib :: Int -> Int
fib n
 | n <= 1       = n
 | otherwise    = fib(n-1) + fib(n-2)

listReverse :: [a] -> [a]
listReverse [] = []
listReverse (n:t) = listReverse t ++ [n]

listAdd :: [Int] -> [Int] -> [Int]
listAdd [] []           = []
listAdd (x:xt) []       = x : listAdd xt []
listAdd [] (y:yt)       = y : listAdd [] yt
listAdd (x:xt) (y:yt)   = x+y : listAdd xt yt

inList :: Eq a => [a] -> a -> Bool
inList [] _ = False 
inList (x:t) v 
 | x == v       = True
 | otherwise    = inList t v