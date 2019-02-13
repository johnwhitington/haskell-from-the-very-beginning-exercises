map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


data Rect a = Square a
            | Rectangle a a deriving Show


area :: Num a => Rect a -> a

area (Square s) = s * s
area (Rectangle w h) = w * h


rotate :: Ord a => Rect a -> Rect a

rotate (Rectangle w h) =
  if w > h then Rectangle h w else Rectangle w h
rotate (Square s) = Square s


widthOfRect :: Rect a -> a

widthOfRect (Square s) = s
widthOfRect (Rectangle w _) = w


rectCompare :: Ord a => Rect a -> Rect a -> Bool

rectCompare a b =
  widthOfRect a < widthOfRect b


insert :: (a -> a -> Bool) -> a -> [a] -> [a]

insert f x [] = [x]
insert f x (h:t) =
  if f x h then x : h : t else h : insert f x t


sort :: (a -> a -> Bool) -> [a] -> [a]

sort f [] = []
sort f (h:t) = insert f h (sort f t)


pack :: Ord a => [Rect a] -> [Rect a]

pack rs =
  sort rectCompare (map' rotate rs)


data Sequence a = Nil | Cons a (Sequence a) deriving Show


seqTake :: (Eq a, Num a) => a -> Sequence b -> Maybe (Sequence b)

seqTake 0 _ = Just Nil
seqTake _ Nil = Nothing
seqTake n (Cons h t) =
  case seqTake (n - 1) t of
    Nothing -> Nothing
    Just l -> Just (Cons h l)


seqDrop :: (Eq a, Num a) => a -> Sequence b -> Maybe (Sequence b)

seqDrop 0 l = Just l
seqDrop _ Nil = Nothing
seqDrop n (Cons _ t) = seqDrop (n - 1) t


seqMap :: (a -> b) -> Sequence a -> Sequence b

seqMap _ Nil = Nil
seqMap f (Cons h t) = Cons (f h) (seqMap f t)


power :: (Eq b, Num a, Num b) => a -> b -> a

power x 0 = 1
power x 1 = x
power x n = x * power x (n - 1)


data Expr a = Num a
            | Add (Expr a) (Expr a)
            | Subtract (Expr a) (Expr a)
            | Multiply (Expr a) (Expr a)
            | Divide (Expr a) (Expr a)
            | Power (Expr a) (Expr a) deriving Show


evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Multiply e e') = evaluate e - evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'
evaluate (Power e e') = power (evaluate e) (evaluate e')

