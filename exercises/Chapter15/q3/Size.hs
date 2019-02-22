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


length' :: Num b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: Num a => [a] -> a

sum' [] = 0
sum' (x:xs) = x + sum' xs


linesOfFile :: Handle -> IO [String]

linesOfFile fh =
  do finished <- hIsEOF fh
     if finished then return [] else
       do x <- hGetLine fh
          xs <- linesOfFile fh
          return (x : xs)


numChars :: Num a => FilePath -> IO a

numChars inFile =
  do inHandle <- openFile inFile ReadMode
     lines <- linesOfFile inHandle
     hClose inHandle
     return (sum' (map' length' lines)) 


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile] ->
         do size <- numChars inFile
            putStrLn (show size)
       _ ->
         putStrLn "Usage: Size filename"

