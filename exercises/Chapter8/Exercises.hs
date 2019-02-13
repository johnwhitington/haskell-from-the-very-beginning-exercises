replace :: Eq a => a -> b -> [(a, b)] -> Maybe [(a, b)]

replace k v [] = Nothing
replace k v ((k', v'):t) =
  if k == k' then Just ((k, v) : t) else
    case replace k v t of
      Just l -> Just ((k', v') : l)
      Nothing -> Nothing


makeDict :: [a] -> [b] -> Maybe [(a, b)]

makeDict [] [] = Just []
makeDict _  [] = Nothing
makeDict [] _  = Nothing
makeDict (k:ks) (v:vs) =
  case makeDict ks vs of
     Nothing -> Nothing
     Just l -> Just ((k, v) : l)


makeLists :: [(a, b)] -> ([a], [b])

makeLists [] = ([], [])
makeLists ((k, v):t) =
  let (ks, vs) = makeLists t in
    (k : ks, v : vs)


member :: Eq a => a -> [a] -> Bool

member x [] = False
member x (h:t) = x == h || member x t


dictionaryOfPairsInner :: Eq a => [a] -> [(a, b)] -> [(a, b)]

dictionaryOfPairsInner keysSeen [] = []
dictionaryOfPairsInner keysSeen ((k, v):t) =
   if member k keysSeen
     then dictionaryOfPairsInner keysSeen t
     else (k, v) : dictionaryOfPairsInner (k : keysSeen) t


dictionaryOfPairs :: Eq a => [(a, b)] -> [(a, b)]

dictionaryOfPairs l =
  dictionaryOfPairsInner [] l


add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v'):t) =
  if k == k' then (k, v) : t else (k', v') : add k v t


union :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]

union [] b = b
union ((k, v):t) b = add k v (union t b)

