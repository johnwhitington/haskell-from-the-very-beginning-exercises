--Q7 out of order, since we need reverse' later.
revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (x:xs) = revInner (x : a) xs


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


evenElements :: [a] -> [a]

evenElements [] = []
evenElements [_] = []
evenElements (_:x:xs) = x : evenElements xs


-- Haskell won't let us re-use a name in a script, so we call this evenElements2
evenElements2 :: [a] -> [a]

evenElements2 (_:x:xs) = x : evenElements2 xs
evenElements2 l = []


countTrue :: Num a => [Bool] -> a

countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs


makePalindrome :: [a] -> [a]

makePalindrome l =
  l ++ reverse' l


isPalindrome :: Eq a => [a] -> Bool

isPalindrome l =
  l == reverse' l


dropLast :: [a] -> [a]

dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs


elem' :: Eq a => a -> [a] -> Bool

elem' e [] = False
elem' e (x:xs) = x == e || elem' e xs


makeSet :: Eq a => [a] -> [a]

makeSet [] = []
makeSet (x:xs) = if elem' x xs then makeSet xs else x : makeSet xs


answer1 :: Integral a => [a]

answer1 = [x | x <- [1 .. 9999], x `rem` 21 == 0 && x `rem` 83 == 0]


answer2 :: Integral a => [a]

answer2 = [x | x <- [1 .. 9999], x `rem` 21 == 0 || x `rem` 83 == 0]


answer3 :: Integral a => [a]

answer3 = [x | x <- [1 .. 9999], x `rem` 21 == 0, x `rem` 83 == 0]


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


countTrue3 l = length' [x | x <- l, x == True]


countTrue4 l = length' [x | x <- l, x]

