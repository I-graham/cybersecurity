install:
	sudo apt-get install haskell-platform

frequency:
	ghc mono_alphabetic.hs -o decoder.out && ./decoder.out frequency $(ARGS)

decode:
	ghc mono_alphabetic.hs -o decoder.out && ./decoder.out decode $(ARGS)