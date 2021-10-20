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
  let func = if mode == "human" then human else numOut
  writeFile "output" (func key text)

xorNums :: String -> String -> [Int]
xorNums key = zipWith (xor `on` ord) ((concat . repeat) key)

numOut :: String -> String -> String
numOut key = tail . foldr (\a -> (" " ++) . showHex a) "" . xorNums key

human :: String -> String -> String
human key text = map chr (xorNums key text)