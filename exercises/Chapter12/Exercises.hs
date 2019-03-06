between :: (Fractional a, Fractional b) => (a, b) -> (a, b) -> (a, b)

between (x, y) (x', y') =
  ((x + x') / 2, (y + y') / 2)


roundNum :: (Integral b, RealFrac a) => a -> b

roundNum x =
  if fromIntegral c - x <= x - fromIntegral f then c else f
    where c = ceiling x
          f = floor x


parts :: RealFrac a => a -> (Integer, a)

parts x =
  if x < 0 then
    let (a, b) = parts (- x) in
      (- a, b)
  else
    (floor x, x - fromIntegral (floor x))


replicate' :: (Eq a, Num a) => a -> b -> [b]

replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x


makeLine :: (Num a, Eq a) => a -> String

makeLine x =
  replicate' x ' ' ++ ['*', '\n']


star :: RealFrac a => a -> String

star x =
  let i = floor (x * 50) in
    let i' = if i == 50 then 49 else i in
      makeLine (if i' == 0 then 0 else i' - 1)


plot :: (Ord a, Num a, RealFrac b) => (a -> b) -> a -> a -> a -> String

plot f a b dy =
  if a > b then
    []
  else
    star (f a) ++ plot f (a + dy) b dy
 
-- example: putStr (plot sin 0 pi (pi / 20))

