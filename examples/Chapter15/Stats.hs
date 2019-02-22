-- Text Statistics
import System.Environment
import Textstat

main :: IO ()

main =
  do args <- getArgs
     case args of
       [inFile] ->
          do (l, c, w, s) <- Textstat.statsFromFile inFile
             putStr "Lines: "
             putStrLn (show l)
             putStr "Characters: "
             putStrLn (show c)
             putStr "Words: "
             putStrLn (show w)
             putStr "Sentences: "
             putStrLn (show s)
       _ -> putStrLn "Usage: Stats <filename>"

