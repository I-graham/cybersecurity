module Main where

import Data.Bits (xor)
import System.Environment (getArgs)

main :: IO ()
main = do
  [mode, textFile, keyFile] <- getArgs
  text <- readFile textFile
  key <- readFile keyFile
  putStrLn $ case mode of
    "human" -> human text key
    "numOut" -> numOut text key
    _ -> "Invalid"

human :: String -> String -> String
human text key = "!"

numOut :: String -> String -> String
numOut text key = "!"