insert :: [Int] -> Int -> [Int] 
insert [] y = [y]
insert (x:xs) y
    | y <= x = y:x:xs
    | otherwise = x : insert xs y 

isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove (x1:xs) y
    | x1 == y = xs
    | otherwise = x1 : remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort x = minimum x : ssort (remove x (minimum x))
    where 
        minimum [x] = x
        minimum (x1:xs)
            | x1 < minimum xs = x1
            | otherwise = minimum xs

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x1:xs) (y1:ys)
    | x1 < y1 = x1:merge xs (y1:ys)
    | otherwise = y1: merge (x1:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort x = merge (msort (take half x)) (msort (drop half x))
    where 
        l = length x
        half = l `div` 2

qsort :: [Int] -> [Int] 
qsort [] = []
qsort [x] = [x]
qsort (x1:xs) = qsort (smaller x1 xs) ++ [x1] ++ qsort (greater x1 xs)
    where
        smaller _ [] = []
        smaller x (y1:ys)
            | x > y1 = y1 : smaller x ys
            | otherwise = smaller x ys
        greater _ [] = []
        greater x (y1:ys)
            | x <= y1 = y1 : greater x ys
            | otherwise = greater x ys
        
genQsort :: Ord a => [a] -> [a] 
genQsort [] = []
genQsort [x] = [x]
genQsort (x1:xs) = genQsort (smaller x1 xs) ++ [x1] ++ genQsort (greater x1 xs)
    where
        smaller _ [] = []
        smaller x (y1:ys)
            | x > y1 = y1 : smaller x ys
            | otherwise = smaller x ys
        greater _ [] = []
        greater x (y1:ys)
            | x <= y1 = y1 : greater x ys
            | otherwise = greater x ys