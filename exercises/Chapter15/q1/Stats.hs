-- Text Statistics
import System.Environment
import Textstat


printHistogramList :: [(Char, Integer)] -> IO ()

printHistogramList [] = return ()
printHistogramList ((k, v):t) =
  do putStr "For character "
     putStr (show k)
     putStr " the count is "
     putStr (show v)
     putStrLn "."
     printHistogramList t


printHistogram :: Textstat.Tree (Char, Integer) -> IO ()

printHistogram tree =
  printHistogramList (mergeSort (listOfTree tree))


main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile] ->
          do (l, c, w, s, h) <- Textstat.statsFromFile inFile
             putStr "Lines: "
             putStrLn (show l)
             putStr "Characters: "
             putStrLn (show c)
             putStr "Words: "
             putStrLn (show w)
             putStr "Sentences: "
             putStrLn (show s)
       _ -> putStrLn "Usage: stats <filename>"

