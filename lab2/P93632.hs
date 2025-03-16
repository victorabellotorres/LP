eql :: [Int] -> [Int] -> Bool
eql x y
    | length x /= length y = False
    | otherwise = all id (zipWith (==) x y)

prod :: [Int] -> Int
prod x = foldl (*) 1 x

prodOfEvens :: [Int] -> Int
prodOfEvens x = foldl (*) 1 (filter (\x -> mod x 2 == 0) x)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = foldr (+)  0 $ zipWith (*) x y