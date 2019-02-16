import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (h:t) = revInner (h : a) t


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (h:t) =
  if f h
    then h : filter' f t
    else filter' f t


linesOfFile :: Handle -> IO [String]

linesOfFile h =
  do finished <- hIsEOF h
     if finished then return [] else
       do x <- hGetLine h
          rest <- linesOfFile h
          return (x : rest)


matches1 :: String -> String -> Bool

matches1 [] _ = True
matches1 _ [] = False 
matches1 (x:xs) (y:ys) = x == y && matches1 xs ys


matches :: String -> String -> Bool

matches [] [] = True
matches _ [] = False
matches term (l:ls) = matches1 term (l : ls) || matches term ls


printStrings :: [String] -> IO ()

printStrings [] = return ()
printStrings (h:t) =
  do putStrLn h
     printStrings t


search :: FilePath -> String -> IO ()

search inFile searchString =
  do inHandle <- openFile inFile ReadMode
     lines <- linesOfFile inHandle
     let matched = filter' (matches searchString) lines
     printStrings matched
     hClose inHandle


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, searchString] -> search inFile searchString
       _ -> putStrLn "Usage: search <filename> <search string>"

