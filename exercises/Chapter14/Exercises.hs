import Text.Read
import System.IO


printIntegersInner :: Show a => [a] -> IO ()

printIntegersInner [] = return ()
printIntegersInner [x] =
  putStr (show x)
printIntegersInner (x:xs) =
  do putStr (show x)
     putStr ", "
     printIntegersInner xs


printIntegers :: Show a => [a] -> IO ()

printIntegers l =
  do putStr "["
     printIntegersInner l
     putStr "]"


getIntegerMaybe :: IO (Maybe Integer)

getIntegerMaybe =
  do line <- getLine
     return (readMaybe line :: Maybe Integer)


readThree :: IO (Integer, Integer, Integer)

readThree =
  do x <- getIntegerMaybe
     y <- getIntegerMaybe
     z <- getIntegerMaybe
     case (x, y, z) of
       (Just a, Just b, Just c) -> return (a, b, c)
       _ ->
         do putStrLn "Not valid integers. Please try again"
            readThree


readDictNumber :: Integer -> IO [(Integer, String)]

readDictNumber n =
  if n == 0 then return [] else
    do i <- getIntegerMaybe
       name <- getLine
       case i of
         Nothing ->
           do putStrLn "Not a valid integer."
              readDictNumber n
         Just x ->
           do rest <- readDictNumber (n - 1)
              return ((x, name) : rest)


readDict :: IO [(Integer, String)]

readDict =
  do putStrLn "How many dictionary entries to input?"
     n <- getIntegerMaybe
     case n of
       Nothing ->
         do putStrLn "Not a number."
            readDict
       Just i ->
         if i < 0 then
            do putStrLn "Number is negative."
               readDict
         else
           readDictNumber i 


map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


row :: Handle -> [Integer] -> IO ()

row fh [] = return ()
row fh (x:xs) =
  do hPutStr fh (show x)
     hPutStr fh "\t"
     row fh xs


rows :: Handle -> Integer -> [Integer] -> IO ()

rows fh n [] = return ()
rows fh n (x:xs) =
  do row fh (map' (* x) [1 .. n])
     hPutStr fh "\n"
     rows fh n xs


table :: FilePath -> Integer -> IO ()

table filename n =
  do fh <- openFile filename WriteMode
     rows fh n [1 .. n]
     hClose fh


countLinesHandle :: Num a => Handle -> IO a

countLinesHandle fh =
  do e <- hIsEOF fh
     if e then return 0 else
       do hGetLine fh
          r <- countLinesHandle fh
          return (1 + r)


countLines :: Num a => FilePath -> IO a

countLines fileName =
  do fh <- openFile fileName ReadMode
     lines <- countLinesHandle fh
     hClose fh
     return lines


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

