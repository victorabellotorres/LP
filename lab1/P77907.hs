absValue :: Int -> Int
absValue n
    | n >= 0    = n
    | otherwise = -n

power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n-1)

havedivisor ::Int -> Int -> Bool
havedivisor n divisor
    | divisor <= 1 = False
    | mod n divisor == 0 = True
    | otherwise = havedivisor n (divisor-1)


isPrime :: Int -> Bool
isPrime n
    | n == 0    = False
    | n == 1    = False
    | otherwise = not (havedivisor n (div n 2))
    | otherwise = not (divisors 2)
    where divisors i
    |