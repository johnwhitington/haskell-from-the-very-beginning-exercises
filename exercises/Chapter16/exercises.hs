import Data.Char
import Data.List
import Data.String

process :: String -> String

process s = map toLower s


process2 :: String -> String

process2 = map toLower


joinNames :: [String] -> [String] -> [String]

joinNames firsts seconds =
  zipWith
    (++)
    firsts
    (zipWith (++) (replicate (length seconds) " ") seconds)


addNums :: Num a => [a] -> [a] -> [a]

addNums = zipWith (+)


isolate :: (Ord a, Num a) => [a] -> [a]

isolate l =
  takeWhile (> 0) (dropWhile (<= 0) l)


f :: String -> String

f s = unwords (reverse (map reverse (words s)))

f2 :: String -> String

f2 = unwords . reverse . (map reverse) . words


toLower2 c | c >= 'A' && c <= 'Z'  = chr (ord c + 32)
           | otherwise = c

toLower3 c | c >= 'A' && c <= 'Z'  = chr (ord c - ord 'A' + ord 'a')
           | otherwise = c

