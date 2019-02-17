--Haskell won't let us define a constructor multiple times in a script, so we
--leave these in comments 

--data Colour = Red | Green | Blue | Yellow
--
--data Colour = Red | Green | Blue | Yellow deriving Show
--
--col :: Colour
--
--col = Blue
--
--cols :: [Colour]
--
--cols = [Red, Red, Green, Yellow]
--
--colpair :: (Char, Colour)
--
--colpair = ('R', Red)


data Colour a = Red
              | Green
              | Blue
              | Yellow
              | RGB a a a deriving Show


cols :: Num a => [Colour a]

cols = [Red, Red, Green, Yellow, RGB 150 0 255]


components :: Num a => Colour a -> (a, a, a)

components Red = (255, 0, 0)
components Green = (0, 255, 0)
components Blue = (0, 0, 255)
components Yellow = (255, 255, 0)
components (RGB r g b) = (r, g, b)


data Sequence a = Nil | Cons a (Sequence a) deriving Show


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


append :: [a] -> [a] -> [a]

append [] y = y
append (x:xs) y = x : append xs y


seqLength :: Num b => Sequence a -> b

seqLength Nil = 0
seqLength (Cons _ xs) = 1 + seqLength xs


seqAppend :: Sequence a -> Sequence a -> Sequence a

seqAppend Nil ys = ys
seqAppend (Cons x xs) ys = (Cons x (seqAppend xs ys))


data Expr a = Num a
            | Add (Expr a) (Expr a)
            | Subtract (Expr a) (Expr a)
            | Multiply (Expr a) (Expr a)
            | Divide (Expr a) (Expr a) deriving Show


evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Subtract e e') = evaluate e - evaluate e'
evaluate (Multiply e e') = evaluate e * evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'

