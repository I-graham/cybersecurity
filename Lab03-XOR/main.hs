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
  putStrLn $ case mode of
    "human" -> human key text
    "numOut" -> numOut key text
    _ -> "Invalid"

xorNums :: String -> String -> [Int]
xorNums key text = zipWith (xor `on` ord) text $ (concat . repeat) key

numOut :: String -> String -> String
numOut key text = foldr (\a -> showHex a . (" " ++)) "" $ xorNums key text

human :: String -> String -> String
human key text = map chr (xorNums key text)