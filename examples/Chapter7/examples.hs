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


safeTake :: (Eq a, Num a) => a -> [b] -> Maybe [b]

safeTake 0 _ = Just []
safeTake _ [] = Nothing
safeTake n l = Just (take' n l)


safeDrop :: (Eq a, Num a) => a -> [b] -> Maybe [b]

safeDrop 0 x = Just x
safeDrop n [] = Nothing
safeDrop n (h:t) = Just (drop' n t)


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

