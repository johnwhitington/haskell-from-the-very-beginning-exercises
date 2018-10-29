import Data.Char
import Data.List
import Data.String

-- Question 1
process :: String -> String

process s = map toLower s


process2 :: String -> String

process2 = map toLower


-- Question 2
joinNames :: [String] -> [String] -> [String]

joinNames firsts seconds =
  zipWith
    (++)
    firsts
    (zipWith (++) (replicate (length seconds) " ") seconds)


addNums :: Num a => [a] -> [a] -> [a]

addNums = zipWith (+)


-- Question 3
isolate :: (Ord a, Num a) => [a] -> [a]

isolate l =
  takeWhile (> 0) (dropWhile (<= 0) l)


-- Question 5

f :: String -> String

f s = unwords (reverse (map reverse (words s)))

f2 :: String -> String

f2 = unwords . reverse . (map reverse) . words


-- Question 6
toLower2 c | c >= 'A' && c <= 'Z'  = chr (ord c + 32)
           | otherwise = c

toLower3 c | c >= 'A' && c <= 'Z'  = chr (ord c - ord 'A' + ord 'a')
           | otherwise = c

