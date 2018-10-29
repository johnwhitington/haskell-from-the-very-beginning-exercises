import System.Environment
import System.IO

revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (h:t) = revInner (h : a) t


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


linesOfFile :: [String] -> Handle -> IO [String]

linesOfFile l h =
  do finished <- hIsEOF h
     if finished then return (reverse' l) else
       do x <- hGetLine h
          rest <- linesOfFile (x:l) h
          return (reverse' rest)


writeLines :: Handle -> [String] -> IO ()

writeLines h [] = return ()
writeLines h (x:xs) =
  do hPutStrLn h x
     writeLines h xs


reverseLines :: FilePath -> FilePath -> IO ()

reverseLines inFile outFile =
  do inHandle <- openFile inFile ReadMode
     outHandle <- openFile outFile WriteMode
     lines <- linesOfFile [] inHandle
     writeLines outHandle (reverse' lines)
     hClose inHandle
     hClose outHandle


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, outFile] -> reverseLines inFile outFile
       _ -> do putStrLn "Usage: revlines input_filename output_filename"
