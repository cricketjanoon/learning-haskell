import Data.Char

capitalize :: Char -> Char
capitalize ch
    | ('a' <= ch && ch <= 'z') = chr (ord ch + (ord 'A' - ord 'a'))
    | otherwise = ch


occurs :: Char -> String -> Bool
occurs c "" = False
occurs c (x:xs)
    | c==x = True
    | otherwise = occurs c xs


touppercase :: String -> String
touppercase "" = ""
touppercase (c:cs) = (capitalize c):(touppercase cs)


position :: Char -> String -> Int
position c "" = 0
position c (x:xs)
    | c==x = 0
    | otherwise = 1 + (position c xs)


whitespace :: Char -> Bool
whitespace ' ' = True
whitespace '\n' = True
whitespace '\t' = True
whitespace _ = False


-- not enough, don't handle multiple spaces
wscount :: String -> Int
wscount "" = 1
wscount (x:xs)
    | whitespace x = 1 + wscount xs
    | otherwise = wscount xs

wordcaux :: String -> Int
wordcaux [c] = 0
wordcaux (c:d:ds)
    | (whitespace c) && not (whitespace d) = 1 + wordcaux (d:ds)
    | otherwise = wordcaux (d:ds)

wordc :: String -> Int
wordc s = wordcaux (' ':s) 
