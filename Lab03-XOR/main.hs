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

numOut :: String -> String -> String
numOut key text = do
  let nums = zipWith (xor `on` ord) text $ (concat . repeat) key
  foldr ((\a b -> " "++ a b) . showHex) "" nums

human :: String -> String -> String
human key text = "!"