-- Eucli'd Algorithm for gcd(a, b), assume  a>=b
--   If b is 0, define the answer to be a
--   Otherwise, gcd(a, b) = gcd(b, mod a b)

my_gcd :: Int -> Int -> Int
my_gcd a 0 = a
my_gcd a b
    | a>=b = my_gcd b (mod a b)
    | otherwise = my_gcd b a