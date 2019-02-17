import System.IO
import Text.Read


printDictEntry :: Show a => (a, String) -> IO ()

printDictEntry (k, v) =
  do putStrLn (show k)
     putStrLn v


printDict :: Show a => [(a, String)] -> IO ()

printDict [] = return ()
printDict (x:xs) =
  do printDictEntry x
     printDict xs


getInteger :: IO Integer

getInteger =
  do line <- getLine
     return (read line :: Integer)


readDict :: IO [(Integer, String)]

readDict =
  do i <- getInteger
     if i == 0 then return [] else
       do name <- getLine
          dict <- readDict
          return ((i, name) : dict)


getIntegerMaybe :: IO (Maybe Integer)

getIntegerMaybe =
  do line <- getLine
     return (readMaybe line :: Maybe Integer)


readDictRobust :: IO [(Integer, String)]

readDictRobust =
  do i <- getIntegerMaybe
     case i of
       Just 0 -> return []
       Just x ->
         do name <- getLine
            dict <- readDictRobust
            return ((x, name) : dict)
       Nothing ->
         do putStrLn "Not a number. Try again."
            x <- readDictRobust
            return x


entryToHandle :: Show a => Handle -> (a, String) -> IO ()

entryToHandle fh (k, v) =
  do hPutStrLn fh (show k)
     hPutStrLn fh v


dictionaryToHandle :: Show a => Handle -> [(a, String)] -> IO ()

dictionaryToHandle fh [] = return ()
dictionaryToHandle fh (x:xs) =
  do entryToHandle fh x
     dictionaryToHandle fh xs


dictionaryToFile :: Show a => FilePath -> [(a, String)] -> IO ()

dictionaryToFile filename dict =
  do fh <- openFile filename WriteMode
     dictionaryToHandle fh dict
     hClose fh


entryOfHandle :: Handle -> IO (Maybe (Integer, String))

entryOfHandle fh =
  do k <- hGetLine fh
     v <- hGetLine fh
     case readMaybe k :: Maybe Integer of
       Nothing -> return Nothing
       Just k' -> return (Just (k', v))


dictionaryOfHandle :: Handle -> IO (Maybe [(Integer, String)])

dictionaryOfHandle fh =
  do ended <- hIsEOF fh
     if ended then return (Just []) else
       do x <- entryOfHandle fh
          case x of
            Nothing -> return Nothing
            Just x' ->
              do xs <- dictionaryOfHandle fh
                 case xs of
                   Nothing -> return Nothing
                   Just xs' -> return (Just (x' : xs'))


dictionaryOfFile :: FilePath -> IO (Maybe [(Integer, String)])
              
dictionaryOfFile filename =
  do fh <- openFile filename ReadMode
     dict <- dictionaryOfHandle fh
     hClose fh
     return dict


-- Extended example: text file statistics

handleStatistics :: (Show a, Num a) => Handle -> a -> IO ()

handleStatistics fh lines =
  do ended <- hIsEOF fh
     if ended then
       do putStr "There were "
          putStr (show lines)
          putStrLn " lines."
     else
       do line <- hGetLine fh
          handleStatistics fh (lines + 1)


fileStatistics :: FilePath -> IO ()

fileStatistics fileName =
  do fh <- openFile fileName ReadMode
     handleStatistics fh 0
     hClose fh


-- Second version, with characters and sentences.

length' :: Num a => [b] -> a

length' [] = 0
length' (_:xs) = 1 + length' xs


handleStatistics2 ::
   (Show a, Show b, Show c, Show d, Num a, Num b, Num c, Num d) =>
   Handle -> a -> b -> c -> d -> IO ()

handleStatistics2 fh lines characters words sentences =
  do ended <- hIsEOF fh
     if ended then
       do putStr "There were "
          putStr (show lines)
          putStr " lines, making up "
          putStr (show characters)
          putStr " characters with "
          putStr (show words)
          putStr " words in "
          putStr (show sentences)
          putStrLn " sentences."
     else
       do line <- hGetLine fh
          let charCount = length' line
              wordCount = length' (filter (\x -> x == ' ') line)
              sentenceCount =
                length'
                  (filter (\x -> x == '.' || x == '?' || x == '!')
                  line)
          handleStatistics2
            fh (lines + 1) (characters + charCount)
            (words + wordCount) (sentences + sentenceCount)


fileStatistics2 :: FilePath -> IO ()

fileStatistics2 filename =
  do fh <- openFile filename ReadMode
     handleStatistics2 fh 0 0 0 0
     hClose fh


-- Third version. We add a histogram of characters, using the tree code from earlier
take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs


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


data Tree a =
    Br a (Tree a) (Tree a)
  | Lf deriving Show


listOfTree :: Tree a -> [a]

listOfTree (Br x l r) = listOfTree l ++ [x] ++ listOfTree r
listOfTree Lf = []


lookup' :: Ord a => Tree (a, b) -> a -> Maybe b

lookup' Lf _ = Nothing
lookup' (Br (k', v) l r) k =
  if k == k' then Just v else
  if k < k' then lookup' l k else
    lookup' r k


treeInsert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

treeInsert Lf k v = Br (k, v) Lf Lf
treeInsert (Br (k', v') l r) k v =
  if k == k' then Br (k, v) l r else
  if k < k' then Br (k', v') (treeInsert l k v) r else
    Br (k', v') l (treeInsert r k v)


printHistogramList :: (Show a, Show b) => [(a, b)] -> IO ()

printHistogramList [] = return ()
printHistogramList ((k, v):t) =
  do putStr "For character "
     putStr (show k)
     putStr " the count is "
     putStr (show v)
     putStrLn "."
     printHistogramList t


printHistogram  :: (Show a, Show b, Ord a, Ord b) => Tree (a, b) -> IO ()

printHistogram tree =
  printHistogramList (mergeSort (listOfTree tree))


updateHistogram :: (Ord a, Num b) => Tree (a, b) -> [a] -> Tree (a, b)

updateHistogram tr [] = tr
updateHistogram tr (x:xs) =
  case lookup' tr x of
    Nothing ->
      updateHistogram (treeInsert tr x 1) xs
    Just v ->
      updateHistogram (treeInsert tr x (v + 1)) xs


handleStatistics3 ::
  (Show a, Show b, Show c, Show d, Num a, Num b, Num c, Num d) =>
  Handle -> a -> b -> c -> d -> Tree (Char, Integer) -> IO ()

handleStatistics3 fh lines characters words sentences histogram =
  do ended <- hIsEOF fh
     if ended then
       do putStr "There were "
          putStr (show lines)
          putStr " lines, making up "
          putStr (show characters)
          putStr " characters with "
          putStr (show words)
          putStr " words in "
          putStr (show sentences)
          putStrLn " sentences."
          printHistogram histogram
     else
       do line <- hGetLine fh
          let charCount = length' line
              wordCount = length' (filter (\x -> x == ' ') line)
              sentenceCount =
                length'
                  (filter (\x -> x == '.' || x == '?' || x == '!')
                  line)
              histogram' = updateHistogram histogram line
          handleStatistics3
            fh (lines + 1) (characters + charCount)
            (words + wordCount) (sentences + sentenceCount)
            histogram'


fileStatistics3 :: FilePath -> IO ()

fileStatistics3 fileName =
  do fh <- openFile fileName ReadMode
     handleStatistics3 fh 0 0 0 0 Lf
     hClose fh

