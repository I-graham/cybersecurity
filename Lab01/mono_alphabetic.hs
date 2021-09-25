main :: IO ()
main = do
	txt <- (readFile "alice_in_wonderland.txt")
	putStrLn txt