smallestInner :: (Num a, Ord a) => Maybe a -> [a] -> Maybe a

smallestInner Nothing [] = Nothing
smallestInner (Just a) [] = Just a
smallestInner Nothing (h:t) =
  if h > 0
    then smallestInner (Just h) t
    else smallestInner Nothing t
smallestInner (Just a) (h:t) =
  if h > 0 && h < a
    then smallestInner (Just h) t
    else smallestInner (Just a) t


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
mapMaybeDefault f d (h:t) =
  case f h of
    Just x -> x : mapMaybeDefault f d t
    Nothing -> d : mapMaybeDefault f d t


splitEither :: (a -> Either b c) -> [a] -> ([b], [c])

splitEither f [] = ([], [])
splitEither f (h:t) =
  let (ls, rs) = splitEither f t in
    case f h of
      Left l -> (l : ls, rs)
      Right r -> (ls, r : rs)

