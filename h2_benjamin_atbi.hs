sumTailRec :: Num a => [a] -> a
sumTailRec n = sumTailRecAux n 0 

sumTailRecAux :: Num a => [a] -> a -> a
sumTailRecAux [] s = 0
sumTailRecAux (n:nt) s 
    | null nt       = n + s
    | otherwise     = sumTailRecAux nt (n + s)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f s [] = s
myFoldl f s (n:nt) = myFoldl f (f s n) nt

myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f s [] = s
myFoldr f s (n:nt) = f n (myFoldr f s nt)

alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap fodd feven [] = []
alternativeMap fodd feven (n:nt) = fodd n : aMapEven fodd feven nt

aMapEven :: (a -> b) -> (a -> b) -> [a] -> [b]
aMapEven fodd feven [] = []
aMapEven fodd feven (n:nt) = feven n : alternativeMap fodd feven nt

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f lst = foldl (\ac li -> if f li then ac++[li] else ac) [] lst

sumsqeven :: [Int] -> Int
sumsqeven = sum . map (^2) . filter even