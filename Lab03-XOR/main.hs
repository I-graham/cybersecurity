module Main where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.Function (on)
import Numeric (showHex)
import System.Environment (getArgs)

main :: IO ()
main = do
  [mode, keyFile, textFile] <- getArgs
  key <- readFile keyFile
  text <- readFile textFile
  writeFile "output" $ case mode of
    "human" -> human key text
    "numOut" -> numOut key text
    _ -> "Invalid"

xorNums :: String -> String -> [Int]
xorNums key = zipWith (xor `on` ord) ((concat . repeat) key)

numOut :: String -> String -> String
numOut key = tail . foldr (\a -> (" " ++) . showHex a) "" . xorNums key

human :: String -> String -> String
human key text = map chr (xorNums key text)