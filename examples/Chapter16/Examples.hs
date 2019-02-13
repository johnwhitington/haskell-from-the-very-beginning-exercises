printDictEntry :: Show a => (a, String) -> IO ()

printDictEntry (k, v) =
  do putStrLn (show k)
     putStrLn v


doList :: Monad m => (a -> m b) -> [a] -> m ()

doList _ [] = return ()
doList f (h:t) =
  do f h
     doList f t


printDict :: Show a => [(a, String)] -> IO ()

printDict = doList printDictEntry
 
