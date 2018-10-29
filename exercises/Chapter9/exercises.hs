--Prelimineries
length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


not' False = True
not' True = False


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


elem' :: Eq a => a -> [a] -> Bool

elem' e [] = False
elem' e (h:t) = h == e || elem' e t


elemAll :: Eq a => a -> [[a]] -> Bool

elemAll x ls =
  let booleans = map' (elem' x) ls in
    not' (elem' False booleans)


-- Haskell won't let us re-use a name in a script, so we call this length'2
elemAll2 :: Eq a => a -> [[a]] -> Bool

elemAll2 x ls =
  not' (elem' False (map' (elem' x) ls))


elemAll3 :: Eq a => a -> [[a]] -> Bool

elemAll3 x =
  not' . (elem' False) . (map' (elem' x))


mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll f l = map' (map' (map' f)) l


-- Haskell won't let us re-use a name in a script, so we call this length'2
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
firstElt n (h:_) = h


firstElts :: a -> [[a]] -> [a]

firstElts n l = map' (firstElt n) l


addNum n ls = map' (n :) ls

