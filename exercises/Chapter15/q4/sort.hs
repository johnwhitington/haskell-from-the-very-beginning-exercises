import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (h:t) = revInner (h : a) t


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (h:t) = f h : map' f t


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (h:t) = h : take' (n - 1) t


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (h:t) = drop' (n - 1) t


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (hx:tx) (hy:ty) =
  if hx < hy
    then hx : merge tx (hy : ty)
    else hy : merge (hx : tx) ty


mergeSort :: Integral a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l
  in
    merge (mergeSort left) (mergeSort right)


linesOfFile :: Handle -> [String] -> IO [String]

linesOfFile inHandle lines =
  do finished <- hIsEOF inHandle
     if finished then return (reverse' lines) else
       do line <- hGetLine inHandle
          rest <- linesOfFile inHandle (line : lines)
          return (reverse' rest)


linesToFile :: Handle -> [String] -> IO ()

linesToFile _ [] = return ()
linesToFile outHandle (h:t) =
  do hPutStrLn outHandle h
     linesToFile outHandle t


sortLines :: [String] -> [String]

sortLines lines =
  map' show (mergeSort (map' (read :: String -> Integer) lines))


sortNums :: FilePath -> FilePath -> IO ()

sortNums inFile outFile =
  do inHandle <- openFile inFile ReadMode
     outHandle <- openFile outFile WriteMode
     lines <- linesOfFile inHandle []
     linesToFile outHandle (sortLines lines)
     hClose inHandle
     hClose outHandle 


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, outFile] ->
          sortNums inFile outFile
       _ ->
          putStrLn "Usage: size filename"

