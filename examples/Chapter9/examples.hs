--Chapter 9
add :: Num a => a -> a -> a

add x y = x + y


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


mapl :: (a -> b) -> [[a]] -> [[b]]

mapl f [] = []
mapl f (h:t) = map' f h : mapl f t


-- Haskell won't let us re-use a name in a script, so we call this mapl2
mapl2 :: (a -> b) -> [[a]] -> [[b]]

mapl2 f l = map' (map' f) l


mapl3 :: (a -> b) -> [[a]] -> [[b]]

mapl3 f = map' (map' f)


mapl4 :: (a -> b) -> [[a]] -> [[b]]

mapl4 = map' . map'


-- Haskell won't let us re-use a name in a script, so we call this add2
add2 :: Num a => a -> a -> a

add2 = \x -> \y -> x + y

