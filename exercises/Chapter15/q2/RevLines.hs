import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (x:xs) = revInner (x : a) xs


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


linesOfFile :: Handle -> IO [String]

linesOfFile h =
  do finished <- hIsEOF h
     if finished then return [] else
       do x <- hGetLine h
          xs <- linesOfFile h
          return (x : xs)


writeLines :: Handle -> [String] -> IO ()

writeLines h [] = return ()
writeLines h (x:xs) =
  do hPutStrLn h x
     writeLines h xs


reverseLines :: FilePath -> FilePath -> IO ()

reverseLines inFile outFile =
  do inHandle <- openFile inFile ReadMode
     outHandle <- openFile outFile WriteMode
     lines <- linesOfFile inHandle
     writeLines outHandle (reverse' lines)
     hClose inHandle
     hClose outHandle


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, outFile] -> reverseLines inFile outFile
       _ -> do putStrLn "Usage: revlines input_filename output_filename"

