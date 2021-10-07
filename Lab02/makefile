install:
	choco install haskell-dev
	refreshenv

clean:
	rm *.out ; rm *.o ; rm *.hi

encode: vigenere.out
	./vigenere.out encode $(ARGS)

decode: vigenere.out
	./vigenere.out decode $(ARGS)

vigenere.out: vigenere.hs
	ghc vigenere.hs -o vigenere.out