module Main where

import Data.Char (chr, isAlpha, isAscii, isUpper, ord, toLower)
import Data.Function (on)
import Data.Map (Map, fromList, insertWith, intersectionWith)
import qualified Data.Map as M (assocs, foldr, map)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Setup (BooleanFlag)
import System.Environment (getArgs)
import Utils (decode, frequency)
import VigenereSolver (findDistancesBetweenRepeats, vigenere)

main :: IO ()
main = do
  args <- getArgs
  let (h : t) = args
  putStrLn ""
  case h of
    "decode" -> decode' t
    "frequency" -> frequency' t
    "vigenere" -> vigenere' t
    _ -> putStrLn "Invalid input."

decode' :: [String] -> IO ()
decode' args = do
  cipherText <- readFile $ head args
  referenceText <- readFile $ args !! 1

  putStrLn $ snd $ decode referenceText cipherText

frequency' :: [String] -> IO ()
frequency' args = do
  txt <- readFile $ head args
  let freq = frequency txt
  let display (k, f) = putStrLn $ [k :: Char] ++ ":" ++ show f
  mapM_ display freq

vigenere' :: [String] -> IO ()
vigenere' args = do
  cipherText <- readFile $ head args
  referenceText <- readFile $ args !! 1
  let (key, message) = vigenere referenceText cipherText
  putStrLn $ "Key: " ++ key
  putStrLn $ "Message: \n" ++ message