module Utils where

import Data.Char (chr, isAlpha, isAscii, isUpper, ord, toLower)
import Data.Function (on)
import Data.Map (Map, fromList, insertWith, intersectionWith)
import qualified Data.Map as M (assocs, foldr, map)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Setup (BooleanFlag)
import System.Environment as Env (getArgs)

type CountMap = Map Char Int

type DistributionMap = Map Char Double

frequency :: String -> [(Char, Double)]
frequency text = do
  M.assocs (findCharDistribution text)

decode :: String -> String -> (Int, String)
decode referenceText cipherText = do
  let referenceDistribution = findCharDistribution referenceText

  let findError r = do
        let rotText = rotateText r cipherText
        let rotDist = findCharDistribution rotText
        calcError referenceDistribution rotDist

  let errors = zip [0 ..] $ map findError [0 .. 26]
  let selection = fst $ foldr1 (\a b -> if snd a < snd b then a else b) errors
  (26 - selection, rotateText selection cipherText)

findCharDistribution :: String -> DistributionMap
findCharDistribution txt = do
  let lowerAlphabetic = (map toLower . filter isValidChar) txt
  let charCounts = countChars lowerAlphabetic
  let totalChars = M.foldr (+) 0 charCounts
  M.map ((flip (/) `on` fromIntegral) totalChars) charCounts
  where
    emptyMap :: CountMap
    emptyMap = fromList $ zip ['a' .. 'z'] (repeat 0)

    countChars :: String -> CountMap
    countChars = foldl (flip incrementChar) emptyMap

    incrementChar :: Char -> CountMap -> CountMap
    incrementChar c map = insertWith (+) c 1 map

calcError :: DistributionMap -> DistributionMap -> Double
calcError refDist textDist = M.foldr (+) 0 distMap
  where
    distMap = intersectionWith (\a b -> (a - b) ^ 2) refDist textDist

rotateText :: Int -> String -> String
rotateText r = map (rotateChar r)

rotateChar :: Int -> Char -> Char
rotateChar r c = if isValidChar c then chr $ (ord c - a + r) `mod` 26 + a else c
  where
    a = ord (if isUpper c then 'A' else 'a')

isValidChar :: Char -> Bool
isValidChar c = isAscii c && isAlpha c