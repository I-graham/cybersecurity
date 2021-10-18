clean:
	rm *.out ; rm *.o ; rm *.hi

run: main.out
	@./main.out $(ARGS)

main.out: main.hs
	ghc main.hs -o main.out