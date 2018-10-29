--Q7 out of order, since we need reverse' later.
revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (h:t) = revInner (h : a) t


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


evenElements :: [a] -> [a]

evenElements [] = []
evenElements [_] = []
evenElements (_:b:t) = b : evenElements t


-- Haskell won't let us re-use a name in a script, so we call this evenElements2
evenElements2 :: [a] -> [a]

evenElements2 (_:b:t) = b : evenElements2 t 
evenElements2 l = []


countTrue :: Num a => [Bool] -> a

countTrue [] = 0
countTrue (True:t) = 1 + countTrue t
countTrue (False:t) = countTrue t


makePalindrome :: [a] -> [a]

makePalindrome l =
  l ++ reverse' l


isPalindrome :: Eq a => [a] -> Bool

isPalindrome l =
  l == reverse' l


dropLast :: [a] -> [a]

dropLast [] = []
dropLast [_] = []
dropLast (h:t) = h : dropLast t


elem' :: Eq a => a -> [a] -> Bool

elem' e [] = False
elem' e (h:t) = h == e || elem' e t


makeSet :: Eq a => [a] -> [a]

makeSet [] = []
makeSet (h:t) = if elem' h t then makeSet t else h : makeSet t


answer1 :: Integral a => [a]

answer1 = [x | x <- [1 .. 9999], x `mod` 21 == 0 && x `mod` 83 == 0]


answer2 :: Integral a => [a]

answer2 = [x | x <- [1 .. 9999], x `mod` 21 == 0 || x `mod` 83 == 0]


answer3 :: Integral a => [a]

answer3 = [x | x <- [1 .. 9999], x `mod` 21 == 0, x `mod` 83 == 0]


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t 


countTrue3 l = length' [x | x <- l, x == True]


countTrue4 l = length' [x | x <- l, x]

