import Data.Char (chr, isAlpha, isAscii, isUpper, ord, toLower)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Distribution.Simple.Setup (BooleanFlag)
import System.Environment as Env (getArgs)

type CountMap = Map.Map Char Int

type DistributionMap = Map.Map Char Double

emptyMap :: CountMap
emptyMap = Map.fromList $ zip ['a' .. 'z'] (repeat 0)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "decode" -> decode $ tail args
    "frequency" -> frequency $ tail args
    _ -> putStrLn "Invalid input."

frequency :: [String] -> IO ()
frequency args = do
  txt <- readFile $ head args
  let dist = findCharDistribution txt

  let assocs = Map.assocs dist
  let display (k, f) = putStrLn $ [k :: Char] ++ ":" ++ show f
  mapM_ display assocs

decode :: [String] -> IO ()
decode args = do
  cipherText <- readFile $ head args

  referenceText <- readFile $ args !! 1
  let referenceDistribution = findCharDistribution referenceText

  let findError r = do
        let rotText = rotateText r cipherText
        let rotDist = findCharDistribution rotText
        calcError referenceDistribution rotDist

  let errors = zip [0 ..] $ map findError [0 .. 26]
  let selection = fst $ foldr1 (\a b -> if snd a < snd b then a else b) errors

  putStrLn $ rotateText selection cipherText

findCharDistribution :: String -> DistributionMap
findCharDistribution txt = do
  let lowerAlphabetic = (map toLower . filter isValidChar) txt
  let charCounts = countChars lowerAlphabetic
  let totalChars = Map.foldr (+) 0 charCounts
  let mapping = (\n -> on (/) fromIntegral n totalChars) :: Int -> Double
  Map.map mapping charCounts
  where
    countChars :: String -> CountMap
    countChars = foldl (flip incrementChar) emptyMap

    incrementChar :: Char -> CountMap -> CountMap
    incrementChar c map = Map.insert c (1 + Map.findWithDefault 0 c map) map

calcError :: DistributionMap -> DistributionMap -> Double
calcError refDist textDist = Map.foldr (+) 0 distMap
  where
    distMap = Map.intersectionWith (\a b -> (a - b) ^ 2) refDist textDist

rotateText :: Int -> String -> String
rotateText r = do
  map $ rotateChar r

rotateChar :: Int -> Char -> Char
rotateChar r c = if isValidChar c then chr $ (ord c - a + r) `mod` 26 + a else c
  where
    a = ord (if isUpper c then 'A' else 'a')

isValidChar :: Char -> Bool
isValidChar c = isAscii c && isAlpha c