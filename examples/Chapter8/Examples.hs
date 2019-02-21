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

lookup' k' [] = Nothing
lookup' k' ((k, v):xs) =
  if k == k' then Just v else lookup' k' xs


add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v'):xs) =
  if k == k'
    then (k, v) : xs
    else (k', v') : add k v xs


remove :: Eq a => a -> [(a, b)] -> [(a, b)]

remove k [] = []
remove k ((k', v'):xs) =
  if k == k'
    then xs
    else (k', v'):remove k xs


keyExists :: (Eq a, Eq b) => a -> [(a, b)] -> Bool

keyExists k d =
  lookup' k d /= Nothing

