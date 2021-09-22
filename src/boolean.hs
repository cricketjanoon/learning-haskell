xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor b1 b2 = False

my_and :: Bool -> Bool -> Bool
my_and True b = b
my_and False b2 = False

my_or :: Bool -> Bool -> Bool
my_or False False = False
my_or b1 b2 = True