isNil :: [a] -> Bool

isNil [] = True
isNil _ = False


length' :: Num b => [a] -> b

length' [] = 0
length' (h:t) = 1 + length' t


-- Haskell won't let us re-use a name in a script, so we call this length'2

length'2 :: Num b => [a] -> b

length'2 [] = 0
length'2 (_:t) = 1 + length'2 t


sumElts :: Num a => [a] -> a

sumElts [] = 0
sumElts (h:t) = h + sum t


oddElements :: [a] -> [a]

oddElements [] = []
oddElements [a] = [a]
oddElements (a:_:t) = a : oddElements t


oddElements2 :: [a] -> [a]

oddElements2 (a:_:t) = a : oddElements2 t
oddElements2 l = l


append :: [a] -> [a] -> [a]

append [] b = b
append (h:t) b = h : append t b


reverse' :: [a] -> [a]

reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t

