-- Text statistics

module Textstat where

import System.IO

type Stats = (Integer, Integer, Integer, Integer)


length' :: Num a => [b] -> a

length' [] = 0
length' (_:xs) = 1 + length' xs


statsFromChannel :: Handle -> Stats -> IO Stats

statsFromChannel fh (lines, characters, words, sentences) =
  do ended <- hIsEOF fh
     if ended then
       return (lines, characters, words, sentences)
     else
       do line <- hGetLine fh
          let charCount = length' line
              wordCount = length' (filter (\x -> x == ' ') line)
              sentenceCount =
                length'
                  (filter
                    (\x -> x == '.' || x == '?' || x == '!')
                    line)
          statsFromChannel
            fh
            ((lines + 1),
             (characters + charCount),
             (words + wordCount),
             (sentences + sentenceCount))


statsFromFile :: FilePath -> IO Stats

statsFromFile fileName =
  do fh <- openFile fileName ReadMode
     result <- statsFromChannel fh (0, 0, 0, 0)
     hClose fh
     return result

