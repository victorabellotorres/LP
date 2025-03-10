absValue :: Int -> Int
absValue n
    | n >= 0    = n
    | otherwise = (-n)

power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n-1)


isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not (divisors 2)
    where 
        divisors i
            | i*i > n = False
            | mod n i == 0 = True
            | otherwise = divisors (i+1)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n  = slowFib (n-1) + slowFib (n-2)

quickFib :: Int -> Int
quickFib n = fib n 0 1
    where  
        fib 0 a b = a
        fib n a b = fib (n-1) b (b+a)
    
