install:
	choco install haskell-dev
	refreshenv

clean:
	rm *.out ; rm *.o ; rm *.hi

encode:
	ghc vigenere.hs -o vigenere.out && ./vigenere.out encode $(ARGS)

decode:
	ghc vigenere.hs -o vigenere.out && ./vigenere.out decode $(ARGS)
