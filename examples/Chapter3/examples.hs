-- This is a comment (a line beginning with -- ) and is ignored by Haskell

factorial :: (Eq a, Num a) => a -> a

factorial 1 = 1
factorial n = n * factorial (n - 1)


isVowel :: Char -> Bool

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False


gcd' :: Integral a => a -> a -> a

gcd' a 0 = a
gcd' a b = gcd' b (a `rem` b)


sign :: (Ord a, Num a, Num b) => a -> b

sign x | x < 0     = -1
       | x > 0     = 1
       | otherwise = 0

