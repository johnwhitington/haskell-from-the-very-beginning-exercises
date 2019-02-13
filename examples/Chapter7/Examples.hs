x :: Maybe a

x = Nothing


y :: Maybe Char

y = Just 'x'


z :: Maybe [String]

z = Just [['x']]


safeHead :: [a] -> Maybe a

safeHead [] = Nothing
safeHead (h:_) = Just h


safeTail :: [a] -> Maybe [a]

safeTail [] = Nothing
safeTail (_:t) = Just t


safeDiv :: Integral a => a -> a -> Maybe a

safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


safeTake :: (Ord a, Num a) => a -> [b] -> Maybe [b]

safeTake n l =
  if n >= 0 && n <= length' l
    then Just (take' n l)
    else Nothing


safeDrop :: (Ord a, Num a) => a -> [b] -> Maybe [b]

safeDrop n l =
  if n >= 0 && n <= length' l
    then Just (drop' n l)
    else Nothing


mapMaybe :: (a -> Maybe b) -> [a] -> [b]

mapMaybe _ [] = []
mapMaybe f (h:t) =
  case f h of
    Nothing -> mapMaybe f t
    Just r -> r : mapMaybe f t


-- Haskell won't let us re-use a name in a script, so we call this mapMaybe2
mapMaybe2 :: (a -> Maybe b) -> [a] -> [b]

mapMaybe2 _ [] = []
mapMaybe2 f (h:t) =
  case f h of
    Nothing -> rs
    Just r -> r : rs
  where
    rs = mapMaybe2 f t


safeDiv2 :: Integral a => a -> a -> Either String a  

safeDiv2 _ 0 = Left "Division by Zero"
safeDiv2 x y = Right (x `div` y)

