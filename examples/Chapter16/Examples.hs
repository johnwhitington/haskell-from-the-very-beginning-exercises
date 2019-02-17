printDictEntry :: Show a => (a, String) -> IO ()

printDictEntry (k, v) =
  do putStrLn (show k)
     putStrLn v


doList :: Monad m => (a -> m b) -> [a] -> m ()

doList _ [] = return ()
doList f (x:xs) =
  do f x
     doList f xs


printDict :: Show a => [(a, String)] -> IO ()

printDict = doList printDictEntry
 
