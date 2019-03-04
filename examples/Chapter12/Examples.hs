gcd' :: Integral a => a -> a -> a

gcd' a b =
  if b == 0 then a else gcd' b (a `rem` b)


makeVector :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)

makeVector (x0, y0) (x1, y1) =
  (x1 - x0, y1 - y0)


vectorLength :: Floating a => (a, a) -> a

vectorLength (x, y) =
  sqrt (x * x + y * y)


offsetPoint :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)

offsetPoint (x, y) (px, py) =
  (px + x, py + y)


scaleToLength :: (Eq a, Floating a) => a -> (a, a) -> (a, a)

scaleToLength l (a, b) =
  if currentLength == 0 then (a, b) else (a * factor, b * factor)
    where currentLength = vectorLength (a, b)
          factor = l / currentLength

