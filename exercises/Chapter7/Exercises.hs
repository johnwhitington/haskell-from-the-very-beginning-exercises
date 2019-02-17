smallestInner :: (Num a, Ord a) => Maybe a -> [a] -> Maybe a

smallestInner Nothing [] = Nothing
smallestInner (Just a) [] = Just a
smallestInner Nothing (x:xs) =
  if x > 0
    then smallestInner (Just x) xs
    else smallestInner Nothing xs
smallestInner (Just a) (x:xs) =
  if x > 0 && x < a
    then smallestInner (Just x) xs
    else smallestInner (Just a) xs


smallest :: (Num a, Ord a) => [a] -> Maybe a

smallest x = smallestInner Nothing x


smallest0 :: (Num a, Ord a) => [a] -> a

smallest0 l =
  case smallest l of
    Nothing -> 0
    Just a -> a


sqrtInner :: (Num a, Ord a) => a -> a -> a

sqrtInner x n =
  if x * x > n then x - 1 else sqrtInner (x + 1) n


sqrtMain :: (Num a, Ord a) => a -> Maybe a

sqrtMain n =
  if n < 0 then Nothing else Just (sqrtInner 1 n)


mapMaybeDefault :: (a -> Maybe b) -> b -> [a] -> [b]

mapMaybeDefault f _ [] = []
mapMaybeDefault f d (x:xs) =
  case f x of
    Just r -> r : mapMaybeDefault f d xs
    Nothing -> d : mapMaybeDefault f d xs


splitEither :: (a -> Either b c) -> [a] -> ([b], [c])

splitEither f [] = ([], [])
splitEither f (x:xs) =
  let (ls, rs) = splitEither f xs in
    case f x of
      Left l -> (l : ls, rs)
      Right r -> (ls, r : rs)

