doubleList :: Num a => [a] -> [a]

doubleList [] = []
doubleList (h:t) = (h * 2) : doubleList t


evens :: Integral a => [a] -> [Bool]

evens [] = []
evens (h:t) = (h `mod` 2 == 0) : evens t


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


halve :: Integral a => a -> a

halve x = x `div` 2


isEven :: Integral a => a -> Bool

isEven x =
  x `mod` 2 == 0


-- Haskell won't let us re-use a name in a script, so we call this length'2

evens2 :: Integral a => [a] -> [Bool]

evens2 l = map' isEven l


evens3 :: Integral a => [a] -> [Bool]

evens3 l =
  map' (\x -> x `mod` 2 == 0) l


greater :: Ord a => a -> a -> Bool

greater a b =
  a >= b


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]

merge _ [] l = l
merge _ l [] = l
merge cmp (hx:tx) (hy:ty) =
  if cmp hx hy
       then hx : merge cmp tx (hy : ty)
       else hy : merge cmp (hx : tx) ty

mergeSort :: (a -> a -> Bool) -> [a] -> [a]

mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp l =
  let left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l
  in
    merge cmp (mergeSort cmp left) (mergeSort cmp right)


double x = x * 2


quadruple x = (double . double) x


-- Haskell won't let us re-use a name, so we call these operators .+ and .++ here

f .+ g = \x -> f (g x)


(f .++ g) x = f (g x)


(a, b) +++ (c, d) = (a + c, b + d)


