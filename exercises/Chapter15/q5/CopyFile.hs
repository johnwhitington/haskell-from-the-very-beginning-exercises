import System.Environment
import System.IO


copyFileHandle :: Handle -> Handle -> IO ()

copyFileHandle fromHandle toHandle =
  do e <- hIsEOF fromHandle
     if e then return () else
       do line <- hGetLine fromHandle
          hPutStrLn toHandle line
          copyFileHandle fromHandle toHandle


copyFile :: FilePath -> FilePath -> IO ()

copyFile fromName toName =
  do fromHandle <- openFile fromName ReadMode
     toHandle <- openFile toName WriteMode
     copyFileHandle fromHandle toHandle
     hClose fromHandle
     hClose toHandle


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile, outFile] -> copyFile inFile outFile
       _ -> putStrLn "Usage: copyFile in out"

