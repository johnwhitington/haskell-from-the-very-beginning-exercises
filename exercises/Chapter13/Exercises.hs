--For Question 6
import Data.Char

--Prerequisites
map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


from :: Num a => a -> [a]

from x = x : from (x + 1)


interleave :: [a] -> [a] -> [a]

interleave (h:t) l =
  h : interleave l t


doubleFrom :: Num a => a -> [a]

doubleFrom n = n : doubleFrom (n * 2)


doubles :: Num a => [a]

doubles = doubleFrom 1


repeating :: [a] -> [a]

repeating l = l ++ repeating l


fibInner :: Num a => a -> a -> [a]

fibInner x y = x : fibInner y (x + y)


fib :: Num a => [a]

fib = fibInner 0 1


data Tree a = Br a (Tree a) (Tree a)


allFrom :: Num a => [a] -> Tree [a]

allFrom l =
  Br l (allFrom (0 : l)) (allFrom (1 : l))


allOnes :: Num a => Tree [a]

allOnes = allFrom []


makeList :: Tree a -> [a]

makeList (Br x l r) = x : interleave (makeList l) (makeList r) 


unleave :: [a] -> ([a], [a])

unleave (h : h' : t) =
  let (x, y) = unleave t in
    (h : x, h' : y)
unleave (h : _) = ([h], [])
unleave [] = ([], [])


letterString :: Int -> String

letterString n =
  if n <= 26
    then
      [chr (n + 64)]
    else
      letterString ((n - 1) `div` 26) ++
      letterString (((n - 1) `rem` 26) + 1)


alphas :: [String]

alphas = map' letterString [1 ..]

