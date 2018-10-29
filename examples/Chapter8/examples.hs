p :: (Num a, Num b) => (a, b)

p = (1, 4)


q :: Num a => (a, Char)

q = (1, '1')


fst' :: (a, b) -> a

fst' (x, _) = x


snd' :: (a, b) -> b

snd' (_, y) = y


census :: (Num a, Num b) => [(a, b)]

census = [(1, 4), (2, 2), (3, 2), (4, 3), (5, 1), (6, 2)]


lookup' :: Eq a => a -> [(a, b)] -> Maybe b

lookup' x [] = Nothing
lookup' x ((k, v):t) =
  if k == x then Just v else lookup' x t


add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v'):t) =
  if k == k'
    then (k, v) : t
    else (k', v') : add k v t


remove :: Eq a => a -> [(a, b)] -> [(a, b)]

remove k [] = []
remove k ((k', v'):t) =
  if k == k'
    then t
    else (k', v'):remove k t


keyExists :: (Eq a, Eq b) => a -> [(a, b)] -> Bool

keyExists k d =
  lookup' k d /= Nothing

