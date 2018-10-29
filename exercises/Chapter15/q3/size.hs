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


length' :: Num b => [a] -> b

length' [] = 0
length' (_:t) = 1 + length' t


sum' :: Num a => [a] -> a

sum' [] = 0
sum' (h:t) = h + sum' t


linesOfFile :: [String] -> Handle -> IO [String]

linesOfFile l h =
  do finished <- hIsEOF h
     if finished then return (reverse' l) else
       do x <- hGetLine h
          rest <- linesOfFile (x:l) h
          return (reverse' rest)


numChars :: Num a => FilePath -> IO a

numChars inFile =
  do inHandle <- openFile inFile ReadMode
     lines <- linesOfFile [] inHandle
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
         putStrLn "Usage: size filename"
