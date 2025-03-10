myLength :: [Int] -> Int 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x1:xs)
    | x1 >= x2 = x1
    | x1 < x2 = x2
    where 
        x2 = myMaximum xs

average :: [Int] -> Float
average xs = fromIntegral(sum xs) / fromIntegral(myLength xs)


buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome xs = reverse xs ++ xs

-- remove all the ocurrences of the elements of y in x
remove :: [Int] -> [Int] -> [Int]
remove [] y = []
remove x [] = x 
remove (x:xs) ys
    | elem x ys = remove xs ys
    | otherwise = [x] ++ remove xs ys

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs) 
    | (x `mod` 2 /= 0) = ([x] ++ odds, evens)
    | (x `mod` 2 == 0) = (odds, [x] ++ evens)
    where
        (odds, evens) = oddsNevens xs

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not (divisors 2)
    where 
        divisors i
            | i*i > n = False
            | mod n i == 0 = True
            | otherwise = divisors (i+1)

primeDivisors :: Int -> [Int]
primeDivisors x
    | x <= 1 = []
    | otherwise = arePrime x [2..x]
        where 
            arePrime :: Int -> [Int] -> [Int]
            arePrime _ [] = []
            arePrime x (ini:list)
                | x `mod` ini == 0 && isPrime ini = [ini] ++ (arePrime x list)
                | otherwise = arePrime x list