length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t

take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (hx:tx) (hy:ty) =
  if hx < hy
    then hx:merge tx (hy : ty)
    else hy:merge (hx : tx) ty


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
insert x (h:t) =
  if x >= h
    then x : h : t
    else h : insert x t


sort :: Ord a => [a] -> [a]

sort [] = []
sort (h:t) = insert h (sort t)


isSorted :: Ord a => [a] -> Bool

isSorted [] = True
isSorted [x] = True
isSorted (a:b:t) = a <= b && isSorted (b : t)


-- We alter the name because Haskell won't let us use a name twice in a script
isSorted2 :: Ord a => [a] -> Bool

isSorted2 (a:b:t) = a <= b && isSorted2 (b : t)
isSorted2 _ = True


sortComplete :: Ord a => [a] -> [a]

sortComplete l =
  let insert x [] = [x]
      insert x (h:t) =
        if x <= h
          then x : h : t
          else h : insert x t
  in
    case l of
      [] -> []
      (h:t) -> insert h (sortComplete t)

