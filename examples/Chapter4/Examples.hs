isNil :: [a] -> Bool

isNil [] = True
isNil _ = False


length' :: Num b => [a] -> b

length' [] = 0
length' (x:xs) = 1 + length' xs


-- Haskell won't let us re-use a name in a script, so we call this length'2

length'2 :: Num b => [a] -> b

length'2 [] = 0
length'2 (_:xs) = 1 + length'2 xs


sumElts :: Num a => [a] -> a

sumElts [] = 0
sumElts (x:xs) = x + sum xs


oddElements :: [a] -> [a]

oddElements [] = []
oddElements [x] = [x]
oddElements (x:_:xs) = x : oddElements xs


oddElements2 :: [a] -> [a]

oddElements2 (x:_:xs) = x : oddElements2 xs
oddElements2 l = l


append :: [a] -> [a] -> [a]

append [] ys = ys
append (x:xs) ys = x : append xs ys


reverse' :: [a] -> [a]

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (_:xs) = drop' (n - 1) xs

