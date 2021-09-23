appendr :: Int -> [Int] -> [Int]
appendr x [] = [x]
appendr x (y:ys) = y:(appendr x ys)

attach :: [Int] -> [Int] -> [Int]
attach [] l = l
attach (x:xs) l = x:(attach xs l)

my_reverse :: [Int] -> [Int]
my_reverse [] = []
my_reverse (x:xs) = my_reverse (xs) ++ [x]

is_sorted :: [Int] -> Bool
is_sorted [] = True
is_sorted [x] = True
is_sorted (x:y:ys) = (x<=y) && is_sorted (y:ys)

alternating :: [Int] -> Bool
alternating x = (updown x) || (downup x)

updown :: [Int] -> Bool
updown [] = True
updown [x] = True
updown (x:y:ys) = (x>y) && (downup(y:ys))

downup :: [Int] -> Bool
downup [] = True
downup [x] = True
downup (x:y:ys) = (x<y) && (updown(y:ys))

my_take :: Int -> [Int] -> [Int]
my_take n [] = []
my_take n (x:xs)
    | n==0 = []
    | n>0 = x:(my_take(n-1) xs)
    | otherwise = []                -- negative values of n