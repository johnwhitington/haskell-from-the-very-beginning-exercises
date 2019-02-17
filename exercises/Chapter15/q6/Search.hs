import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (x:xs) = revInner (x : a) xs


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs) =
  if f x
    then x : filter' f xs
    else filter' f xs


linesOfFile :: Handle -> IO [String]

linesOfFile h =
  do finished <- hIsEOF h
     if finished then return [] else
       do x <- hGetLine h
          xs <- linesOfFile h
          return (x : xs)


matches1 :: String -> String -> Bool

matches1 [] _ = True
matches1 _ [] = False 
matches1 (x:xs) (y:ys) = x == y && matches1 xs ys


matches :: String -> String -> Bool

matches [] [] = True
matches _ [] = False
matches term (x:xs) = matches1 term (x : xs) || matches term xs


printStrings :: [String] -> IO ()

printStrings [] = return ()
printStrings (x:xs) =
  do putStrLn x
     printStrings xs


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

