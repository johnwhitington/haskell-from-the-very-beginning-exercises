smallest :: (Num a, Ord a) => [a] -> Maybe a

smallest x = s Nothing x where
  s Nothing [] = Nothing
  s (Just a) [] = Just a
  s Nothing (x:xs) =
    if x > 0
      then s (Just x) xs
      else s Nothing xs
  s (Just a) (x:xs) =
    if x > 0 && x < a
      then s (Just x) xs
      else s (Just a) xs

smallest0 :: (Num a, Ord a) => [a] -> a

smallest0 l =
  case smallest l of
    Nothing -> 0
    Just a -> a


sqrtMaybe :: (Num a, Ord a) => a -> Maybe a

sqrtMaybe n =
  if n < 0 then Nothing else Just (s 1 n)
    where
      s x n = if x * x > n then x - 1 else s (x + 1) n

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

