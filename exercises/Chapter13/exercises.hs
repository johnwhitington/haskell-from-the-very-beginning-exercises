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

-- Question 1
doubleFrom :: Num a => a -> [a]

doubleFrom n = n : doubleFrom (n * 2)


doubles :: Num a => [a]

doubles = doubleFrom 1

-- Question 2
repeating :: [a] -> [a]

repeating l = l ++ repeating l

-- Question 3
fibInner :: Num a => a -> a -> [a]

fibInner x y = x : fibInner y (x + y)


fib :: Num a => [a]

fib = fibInner 0 1

-- Question 4
data Tree a = Br a (Tree a) (Tree a)


allFrom :: Num a => [a] -> Tree [a]

allFrom l =
  Br l (allFrom (0 : l)) (allFrom (1 : l))


allOnes :: Num a => Tree [a]

allOnes = allFrom []


makeList :: Tree a -> [a]

makeList (Br x l r) = x : interleave (makeList l) (makeList r) 


-- Question 5
unleave :: [a] -> ([a], [a])

unleave (h : h' : t) =
  let (x, y) = unleave t in
    (h : x, h' : y)
unleave (h : _) = ([h], [])
unleave [] = ([], [])


-- Question 6
letterString :: Int -> String

letterString n =
  if n <= 26
    then
      [chr (n + 64)]
    else
      letterString ((n - 1) `div` 26) ++
      letterString (((n - 1) `mod` 26) + 1)


alphas :: [String]

alphas = map' letterString [1 ..]

