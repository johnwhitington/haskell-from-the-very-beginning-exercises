--Prelimineries
length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


not' :: Bool -> Bool

not' False = True
not' True = False


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


elem' :: Eq a => a -> [a] -> Bool

elem' e [] = False
elem' e (x:xs) = x == e || elem' e xs


elemAll :: Eq a => a -> [[a]] -> Bool

elemAll e ls =
  let booleans = map' (elem' e) ls in
    not' (elem' False booleans)


-- Haskell won't let us re-use a name in a script, so we call this length'2
elemAll2 :: Eq a => a -> [[a]] -> Bool

elemAll2 e ls =
  not' (elem' False (map' (elem' e) ls))


elemAll3 :: Eq a => a -> [[a]] -> Bool

elemAll3 e =
  not' . (elem' False) . (map' (elem' e))


mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll f l = map' (map' (map' f)) l


mapll2 :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll2 f  = map' (map' (map' f))


mapll3 :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll3 = map' . map' . map'


truncateList :: (Ord a, Num a) => a -> [b] -> [b]

truncateList n l =
  if length' l >= n then take' n l else l


truncateLists :: (Num a, Ord a) => a -> [[b]] -> [[b]]

truncateLists n ll = map' (truncateList n) ll


firstElt :: a -> [a] -> a

firstElt n [] = n
firstElt n (x:_) = x


firstElts :: a -> [[a]] -> [a]

firstElts n l = map' (firstElt n) l


addNum n ls = map' (n :) ls

