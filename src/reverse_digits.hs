int_reverse :: Int -> Int
int_reverse n
    | n < 10 = n
    | otherwise = (int_reverse (div n 10)) + (mod n 10)*(power 10 (intlog 10 n))

power :: Int -> Int -> Int
power m 0 = 1
power m n = m * (power m (n-1)) 


intlog :: Int -> Int -> Int
intlog k 1 = 0
intlog k n
    | n >= k = 1 + intlog k (div n k)
    | otherwise = 0