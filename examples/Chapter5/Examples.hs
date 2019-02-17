insert :: Ord a => a -> [a] -> [a]

insert x [] = [x]
insert x (y:ys) =
  if x <= y
    then x : y : ys
    else y : insert x ys


sort :: Ord a => [a] -> [a]

sort [] = []
sort (x:xs) = insert x (sort xs)


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) =
  if x < y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


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

