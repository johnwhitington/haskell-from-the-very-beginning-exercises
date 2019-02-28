replace :: Eq a => a -> b -> [(a, b)] -> Maybe [(a, b)]

replace k v [] = Nothing
replace k v ((k', v'):xs) =
  if k == k' then Just ((k, v) : xs) else
    case replace k v xs of
      Just xs' -> Just ((k', v') : xs')
      Nothing -> Nothing


makeDict :: [a] -> [b] -> Maybe [(a, b)]

makeDict [] [] = Just []
makeDict _  [] = Nothing
makeDict [] _  = Nothing
makeDict (k:ks) (v:vs) =
  case makeDict ks vs of
     Nothing -> Nothing
     Just xs -> Just ((k, v) : xs)


makeLists :: [(a, b)] -> ([a], [b])

makeLists [] = ([], [])
makeLists ((k, v):xs) = (k : ks, v : vs)
  where (ks, vs) = makeLists xs


member :: Eq a => a -> [a] -> Bool

member x [] = False
member x (y:ys) = x == y || member x ys


dictionaryOfPairsInner :: Eq a => [a] -> [(a, b)] -> [(a, b)]

dictionaryOfPairsInner keysSeen [] = []
dictionaryOfPairsInner keysSeen ((k, v):xs) =
   if member k keysSeen
     then dictionaryOfPairsInner keysSeen xs
     else (k, v) : dictionaryOfPairsInner (k : keysSeen) xs


dictionaryOfPairs :: Eq a => [(a, b)] -> [(a, b)]

dictionaryOfPairs l =
  dictionaryOfPairsInner [] l


add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v'):xs) =
  if k == k' then (k, v) : xs else (k', v') : add k v xs


union :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]

union [] ys = ys
union ((k, v):xs) ys = add k v (union xs ys)

