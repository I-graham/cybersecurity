module VigenereSolver where

import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (findIndices, isPrefixOf, mapAccumL, sortBy, tails, transpose)
import Data.Map (Map, assocs, empty, insertWith)
import Data.Text.Lazy.IO (hPutStrLn)
import Distribution.Simple.Setup (CopyDest, emptyTestFlags)
import Utils (decode)

type WordMap = Map String Int

data SearchSettings = SearchSettings {wordLen :: Int, maxWords :: Int}

defaultSettings :: SearchSettings
defaultSettings = SearchSettings {wordLen = 3, maxWords = 3}

vigenere :: String -> String -> (String, String)
vigenere reference cipher = do
  --Created by analyzing frequency of repeated words
  let keyLength = findKeyLength cipher

  let buckets = splitIntoBuckets keyLength cipher
  let decoder = decode reference
  let decoded = map decoder buckets
  let (key, message) = unzip decoded
  (map numToChar key, combineBuckets message)
  where
    numToChar :: Int -> Char
    numToChar n = chr (n + ord 'A')

splitIntoBuckets :: Int -> String -> [String]
splitIntoBuckets n l
  | n > length l = map (\p -> [l !! p | p < length l]) [0 .. n]
  | otherwise = do
    let rest = splitIntoBuckets n (drop n l)
    map (\p -> (l !! p) : (rest !! p)) [0 .. n -1]

combineBuckets :: [String] -> String
combineBuckets texts = concat $ transpose texts

--Functions below this line were used for analysis
findKeyLength :: String -> Int
findKeyLength text = do
  let dists = findDistancesBetweenRepeats text
  foldl1 gcd dists

findDistancesBetweenRepeats :: String -> [Int]
findDistancesBetweenRepeats text = do
  let topRepeats = map fst (findTopRepeats defaultSettings text)
  concatMap (tail . findDistances . findPositions) topRepeats
  where
    findPositions :: String -> [Int]
    findPositions substr = findIndices (substr `isPrefixOf`) (tails text)

    findDistances :: [Int] -> [Int]
    findDistances positions = snd $ mapAccumL (\p1 p2 -> (p2, p2 - p1)) 0 positions

--Used to find repeated subsections, could instead be done manually
--Default settings created based on trial and error
findTopRepeats :: SearchSettings -> String -> [(String, Int)]
findTopRepeats settings text = do
  let repeats = countRepeats settings text
  take (maxWords settings) $ sortBy (flip compare `on` snd) (assocs repeats)

countRepeats :: SearchSettings -> String -> WordMap
countRepeats settings text = do
  let startPositions = [0 .. (length text - wordLen settings)]
  foldl addWordToMap empty startPositions
  where
    addWordToMap ::
      WordMap ->
      Int ->
      WordMap
    addWordToMap map pos = insertWith (+) (getWordAtPos pos) 1 map
    getWordAtPos ::
      Int ->
      String
    getWordAtPos p = slice p (wordLen settings) text

slice :: Int -> Int -> (String -> String)
slice start len = drop start . take (start + len)