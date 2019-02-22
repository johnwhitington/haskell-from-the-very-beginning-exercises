import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (x:xs) = revInner (x : a) xs


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


take' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 l = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: (Eq a, Num a) => a -> [b] -> [b]

drop' 0 l = l
drop' n (_:xs) = drop' (n - 1) xs


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) =
  if x < y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys


mergeSort :: Integral a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let left = take' (length' l `div` 2) l
      right = drop' (length' l `div` 2) l
  in
    merge (mergeSort left) (mergeSort right)


linesOfFile :: Handle -> IO [String]

linesOfFile fh =
  do finished <- hIsEOF fh
     if finished then return [] else
       do x <- hGetLine fh
          xs <- linesOfFile fh
          return (x : xs)


linesToFile :: Handle -> [String] -> IO ()

linesToFile _ [] = return ()
linesToFile fh (x:xs) =
  do hPutStrLn fh x
     linesToFile fh xs


sortLines :: [String] -> [String]

sortLines lines =
  map' show (mergeSort (map' (read :: String -> Integer) lines))


sortNums :: FilePath -> FilePath -> IO ()

sortNums inFile outFile =
  do inHandle <- openFile inFile ReadMode
     outHandle <- openFile outFile WriteMode
     lines <- linesOfFile inHandle
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
          putStrLn "Usage: Sort filename"

