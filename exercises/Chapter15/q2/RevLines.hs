import System.Environment
import System.IO


revInner :: [a] -> [a] -> [a]

revInner a [] = a
revInner a (x:xs) = revInner (x : a) xs


reverse' :: [a] -> [a]

reverse' l =
  revInner [] l


linesOfFile :: Handle -> IO [String]

linesOfFile fh =
  do finished <- hIsEOF fh
     if finished then return [] else
       do x <- hGetLine fh
          xs <- linesOfFile fh
          return (x : xs)


linesToFile :: Handle -> [String] -> IO ()

linesToFile fh [] = return ()
linesToFile fh (x:xs) =
  do hPutStrLn fh x
     linesToFile fh xs


reverseLines :: FilePath -> FilePath -> IO ()

reverseLines inFile outFile =
  do inHandle <- openFile inFile ReadMode
     outHandle <- openFile outFile WriteMode
     lines <- linesOfFile inHandle
     linesToFile outHandle (reverse' lines)
     hClose inHandle
     hClose outHandle


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, outFile] -> reverseLines inFile outFile
       _ -> do putStrLn "Usage: RevLines input_filename output_filename"

