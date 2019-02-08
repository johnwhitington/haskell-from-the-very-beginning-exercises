not' :: Bool -> Bool

not' True = False
not' False = True


sumMatch :: (Eq a, Num a) => a -> a

sumMatch 1 = 1
sumMatch n = n + sumMatch (n - 1)


powerMatch :: (Num a, Num b, Eq b) => a -> b -> a

powerMatch _ 0 = 1
powerMatch x 1 = x
powerMatch x n = x * powerMatch x (n - 1)


-- These names differ slightly from the book, since Haskell won't allow us
-- to define the same name twice in a script.

not'2 :: Bool -> Bool

not'2 x | x == False = True
        | otherwise  = False


not'3 :: Bool -> Bool

not'3 x | x          = False
        | otherwise  = True


not'4 :: Bool -> Bool

not'4 x | x == False = True
        | x == True  = False


sumMatch2 :: (Eq a, Num a) => a -> a

sumMatch2 n | n == 1    = 1
            | otherwise = n + sumMatch2 (n - 1)


powerMatch2 :: (Num a, Num b, Eq b) => a -> b -> a

powerMatch2 x n | n == 0    = 1
                | n == 1    = x
                | otherwise = x * powerMatch2 x (n - 1)


kind :: Num a => Char -> a

kind c | c >= 'a' && c <= 'z' = 0
       | c >= 'A' && c <= 'Z' = 1
       | otherwise            = 2

