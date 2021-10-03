install:
	choco install haskell-dev
	refreshenv

clean:
	rm *.out
	rm *.o
	rm *.hi

frequency:
	ghc decoder.hs -o decoder.out && ./decoder.out frequency $(ARGS)

decode:
	ghc decoder.hs -o decoder.out && ./decoder.out decode $(ARGS)

vigenere:
		ghc decoder.hs -o decoder.out && ./decoder.out vigenere $(ARGS)
