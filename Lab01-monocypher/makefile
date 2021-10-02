install:
	choco install haskell-dev
	refreshenv

frequency:
	ghc mono_alphabetic.hs -o decoder.out && ./decoder.out frequency $(ARGS)

decode:
	ghc mono_alphabetic.hs -o decoder.out && ./decoder.out decode $(ARGS)

vigenere:
		ghc mono_alphabetic.hs -o decoder.out && ./decoder.out vigenere $(ARGS)
