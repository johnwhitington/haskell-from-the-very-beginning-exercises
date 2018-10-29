map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


calm :: String -> String

calm [] = []
calm ('!':t) = '.' : calm t
calm (h:t) = h : calm t


calmChar :: Char -> Char
 
calmChar '!' = '.'
calmChar x = x


-- Haskell won't let us re-use a name in a script, so we call this calm2 
calm2 :: String -> String

calm2 l =
  map' calmChar l


clip :: (Num a, Ord a) => a -> a

clip x =
  if x < 1 then 1 else
    if x > 10 then 10 else x


clip2 :: (Num a, Ord a) => a -> a

clip2 x | x < 1 = 1
        | x > 10 = 10
        | otherwise = x


clipList :: (Num a, Ord a) => [a] -> [a]

clipList l =
  map' clip l


clipList2 :: (Num a, Ord a) => [a] -> [a]

clipList2 l =
  map'
    (\x ->
       if x < 1 then 1 else
         if x > 10 then 10 else x)
    l


apply :: (Eq b, Num b) => (a -> a) -> b -> a -> a

apply f 0 x = x
apply f n x = f (apply f (n - 1) x)


power :: (Eq b, Num b, Num a) => a -> b -> a
 
power a b =
  apply (\x -> x * a) b 1


insert :: (a -> a -> Bool) -> a -> [a] -> [a]

insert f x [] = [x]
insert f x (h:t) =
  if f x h
    then x : h : t
    else h : insert f x t


sort :: (a -> a -> Bool) -> [a] -> [a]

sort f [] = []
sort f (h:t) = insert f h (sort f t)


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (h:t) =
  if f h
    then h : filter' f t
    else filter' f t


all' :: (a -> Bool) -> [a] -> Bool

all' f [] = True
all' f (h:t) = f h && all' f t


mapl :: (a -> b) -> [[a]] -> [[b]]

mapl f [] = []
mapl f (h:t) = map' f h : mapl f t


reverse' :: [a] -> [a]

reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

f l = reverse' (sort (<) (filter' (\x -> x `mod` 15 == 0) l))

f2 l = (reverse' . sort (<)) (filter' (\x -> x `mod` 15 == 0) l)

