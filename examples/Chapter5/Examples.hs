insert :: Ord a => a -> [a] -> [a]

insert x [] = [x]
insert x (h:t) =
  if x <= h
    then x : h : t
    else h : insert x t


sort :: Ord a => [a] -> [a]

sort [] = []
sort (h:t) = insert h (sort t)


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (hx:tx) (hy:ty) =
  if hx < hy
    then hx : merge tx (hy : ty)
    else hy : merge (hx : tx) ty


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l
  in
    merge (mergeSort left) (mergeSort right)


-- Haskell won't let us re-use a name in a script, so we call this mergeSort2

mergeSort2 :: Ord a => [a] -> [a]

mergeSort2 [] = []
mergeSort2 [x] = [x]
mergeSort2 l =
  merge (mergeSort2 left) (mergeSort2 right)
    where
      left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l

