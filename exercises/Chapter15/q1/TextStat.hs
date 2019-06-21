-- Text Statistics
module Textstat where


import System.IO


data Tree a =
    Br a (Tree a) (Tree a)
  | Lf deriving Show


type Stats = (Integer, Integer, Integer, Integer, Tree (Char, Integer))


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs 


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (_:xs) = drop' (n - 1) xs


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs) =
  if f x
    then x : filter' f xs
    else filter' f xs


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) =
  if x < y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys


mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  merge (mergeSort left) (mergeSort right)
    where
      left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l


listOfTree :: Tree a -> [a]

listOfTree (Br x l r) = listOfTree l ++ [x] ++ listOfTree r
listOfTree Lf = []


treeLookup :: Ord a => Tree (a, b) -> a -> Maybe b

treeLookup Lf _ = Nothing
treeLookup (Br (k', v) l r) k =
  if k == k' then Just v else
  if k < k' then treeLookup l k else
    treeLookup r k


insert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

insert Lf k v = Br (k, v) Lf Lf
insert (Br (k', v') l r) k v =
  if k == k' then Br (k, v) l r else
  if k < k' then Br (k', v') (insert l k v) r else
    Br (k', v') l (insert r k v)


updateHistogram :: (Ord a, Num b) => Tree (a, b) -> [a] -> Tree (a, b)

updateHistogram tr [] = tr
updateHistogram tr (x:xs) =
  case treeLookup tr x of
    Nothing ->
      updateHistogram (insert tr x 1) xs
    Just v ->
      updateHistogram (insert tr x (v + 1)) xs


statsFromChannel :: Handle -> Stats -> IO Stats

statsFromChannel fh (lines, characters, words, sentences, histogram) =
  do ended <- hIsEOF fh
     if ended then
       return (lines, characters, words, sentences, histogram)
     else
       do line <- hGetLine fh
          let charCount = length' line
              wordCount = length' (filter' (\x -> x == ' ') line)
              sentenceCount =
                length'
                  (filter'
                     (\x -> x == '.' || x == '?' || x == '!')
                     line)
          statsFromChannel
            fh
             (lines + 1, characters + charCount,
              words + wordCount, sentences + sentenceCount,
              updateHistogram histogram line)


statsFromFile :: FilePath -> IO Stats

statsFromFile fileName =
  do fh <- openFile fileName ReadMode
     result <- statsFromChannel fh (0, 0, 0, 0, Lf)
     hClose fh
     return result

