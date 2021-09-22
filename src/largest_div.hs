largest_div :: Int -> Int
largest_div n = div_search n (n-1)

div_search :: Int -> Int -> Int
div_search m i
    | (mod m i) == 0 = i
    | otherwise = div_search m (i-1)