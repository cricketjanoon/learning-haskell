my_length :: [Int] -> Int
my_length [] = 0
my_length l = 1 + my_length (tail l)


my_length1 :: [Int] -> Int
my_length1 [] = 0
my_length1 (x:xs) = 1 + my_length1 xs


my_sum :: [Int] -> Int
my_sum [] = 0
my_sum (x:xs) = x + my_sum xs