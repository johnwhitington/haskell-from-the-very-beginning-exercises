add :: Num a => a -> a -> a

add x y = x + y


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


mapl :: (a -> b) -> [[a]] -> [[b]]

mapl f [] = []
mapl f (x:xs) = map' f x : mapl f xs


-- Haskell won't let us re-use a name in a script, so we call this mapl2
mapl2 :: (a -> b) -> [[a]] -> [[b]]

mapl2 f l = map' (map' f) l


mapl3 :: (a -> b) -> [[a]] -> [[b]]

mapl3 f = map' (map' f)


mapl4 :: (a -> b) -> [[a]] -> [[b]]

mapl4 = map' . map'


add2 :: Num a => a -> a -> a

add2 = \x -> \y -> x + y

