-- Text Statistics
module Textstat where


import System.IO


data Tree a =
    Br a (Tree a) (Tree a)
  | Lf deriving Show


type Stats = (Integer, Integer, Integer, Integer, Tree (Char, Integer))


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t 


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (h:t) =
  if f h
    then h : filter' f t
    else filter' f t


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (hx:tx) (hy:ty) =
  if hx < hy
    then hx : merge tx (hy : ty)
    else hy : merge (hx : tx) ty


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
updateHistogram tr (h:t) =
  case treeLookup tr h of
    Nothing ->
      updateHistogram (insert tr h 1) t
    Just v ->
      updateHistogram (insert tr h (v + 1)) t


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
             (lines + 1,
              characters + charCount,
              words + wordCount,
              sentences + sentenceCount,
              updateHistogram histogram line)


statsFromFile :: FilePath -> IO Stats

statsFromFile fileName =
  do fh <- openFile fileName ReadMode
     result <- statsFromChannel fh (0, 0, 0, 0, Lf)
     hClose fh
     return result

