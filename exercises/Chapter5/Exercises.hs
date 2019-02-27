length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) =
  if x < y
    then x:merge xs (y : ys)
    else y:merge (x : xs) ys


mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let n = length' l `div` 2 in
    let left = take' n l
        right = drop' n l
    in
      merge (mergeSort left) (mergeSort right)


-- Haskell won't let us re-use a name in a script, so we call this mergeSort2

mergeSort2 :: Ord a => [a] -> [a]

mergeSort2 [] = []
mergeSort2 [x] = [x]
mergeSort2 l =
   merge (mergeSort2 left) (mergeSort2 right)
     where
       n = length' l `div` 2
       left = take' n l
       right = drop' n l


insert :: Ord a => a -> [a] -> [a]

insert x [] = [x]
insert x (y:ys) =
  if x >= y
    then x : y : ys
    else y : insert x ys


sort :: Ord a => [a] -> [a]

sort [] = []
sort (x:xs) = insert x (sort xs)


isSorted :: Ord a => [a] -> Bool

isSorted [] = True
isSorted [x] = True
isSorted (x:x':xs) = x <= x' && isSorted (x' : xs)


-- We alter the name because Haskell won't let us use a name twice in a script
isSorted2 :: Ord a => [a] -> Bool

isSorted2 (x:x':xs) = x <= x' && isSorted2 (x' : xs)
isSorted2 _ = True

-- With where
sortComplete :: Ord a => [a] -> [a]

sortComplete [] = []
sortComplete (x:xs) = insert x (sortComplete xs)
  where
    insert a [] = [a]
    insert a (x:xs) =
      if a <= x then a : x : xs else x : insert a xs
 
-- With let
sortComplete2 :: Ord a => [a] -> [a]

sortComplete2 [] = []
sortComplete2 (x:xs) =
  let insert a [] = [a]
      insert a (x:xs) =
        if a <= x then a : x : xs else x : insert a xs
  in
    insert x (sortComplete2 xs)
