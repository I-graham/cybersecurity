import Data.Char (chr, isAlpha, isAscii, isUpper, ord)
import Data.List (transpose)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  text <- readFile $ args !! 1
  key <- readFile $ args !! 2
  putStrLn ""
  putStrLn $ case head args of
    "decode" -> decode key text
    "encode" -> encode key text
    _ -> "Invalid input."

decode :: String -> String -> String
decode key text = do
  let reverseKey = map (flip rotateChar 'A' . (ord 'A' -) . ord) key
  encode reverseKey text

encode :: String -> String -> String
encode key text = do
  let shiftKeys = map charToShift key
  let buckets = splitIntoBuckets (length key) text
  combineBuckets (zipWith rotateText shiftKeys buckets)
  where
    charToShift :: Char -> Int
    charToShift c = ord c - ord 'A'

--Also used in Lab01
splitIntoBuckets :: Int -> String -> [String]
splitIntoBuckets n l
  | n > length l = take n $ map (: []) l ++ repeat []
  | otherwise = zipWith (:) l $ splitIntoBuckets n (drop n l)

combineBuckets :: [String] -> String
combineBuckets texts = concat $ transpose texts

rotateText :: Int -> String -> String
rotateText r = map (rotateChar r)

rotateChar :: Int -> Char -> Char
rotateChar r c = if isValidChar c then chr $ (ord c - a + r) `mod` 26 + a else c
  where
    a = ord (if isUpper c then 'A' else 'a')

isValidChar :: Char -> Bool
isValidChar c = isAscii c && isAlpha c