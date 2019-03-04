doubleList :: Num a => [a] -> [a]

doubleList [] = []
doubleList (x:xs) = (x * 2) : doubleList xs


evens :: Integral a => [a] -> [Bool]

evens [] = []
evens (x:xs) = (x `rem` 2 == 0) : evens xs


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


halve :: Integral a => a -> a

halve x = x `div` 2


isEven :: Integral a => a -> Bool

isEven x =
  x `rem` 2 == 0


-- Haskell won't let us re-use a name in a script, so we call this evens2

evens2 :: Integral a => [a] -> [Bool]

evens2 l = map' isEven l


evens3 :: Integral a => [a] -> [Bool]

evens3 l =
  map' (\x -> x `rem` 2 == 0) l


greater :: Ord a => a -> a -> Bool

greater a b =
  a >= b


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]

merge _ [] l = l
merge _ l [] = l
merge cmp (x:xs) (y:ys) =
  if cmp x y
       then x : merge cmp xs (y : ys)
       else y : merge cmp (x : xs) ys

mergeSort :: (a -> a -> Bool) -> [a] -> [a]

mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp l =
  merge cmp (mergeSort cmp left) (mergeSort cmp right)
    where left = take' (length' l `div` 2) l
          right = drop' (length' l `div` 2) l


double x = x * 2


quadruple x = (double . double) x


-- Haskell won't let us re-use a name, so we call these operators .+ and .++ here

f .+ g = \x -> f (g x)


(f .++ g) x = f (g x)


(a, b) +++ (c, d) = (a + c, b + d)


