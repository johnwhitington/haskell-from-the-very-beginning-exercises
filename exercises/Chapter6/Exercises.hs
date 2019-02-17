map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


calm :: String -> String

calm [] = []
calm ('!':xs) = '.' : calm xs
calm (x:xs) = x : calm xs


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
insert f x (y:ys) =
  if f x y
    then x : y : ys
    else y : insert f x ys


sort :: (a -> a -> Bool) -> [a] -> [a]

sort f [] = []
sort f (x:xs) = insert f x (sort f xs)


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs) =
  if f x
    then x : filter' f xs
    else filter' f xs


all' :: (a -> Bool) -> [a] -> Bool

all' f [] = True
all' f (x:xs) = f x && all' f xs


mapl :: (a -> b) -> [[a]] -> [[b]]

mapl f [] = []
mapl f (x:xs) = map' f x : mapl f xs


reverse' :: [a] -> [a]

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


f l = reverse' (sort (<) (filter' (\x -> x `rem` 15 == 0) l))


f2 = reverse' . sort (<) . filter' (\x -> x `rem` 15 == 0)

