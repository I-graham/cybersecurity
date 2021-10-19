clean:
	rm *.out ; rm *.o ; rm *.hi

run: main.out
	@./out $(ARGS)

main.out: main.hs
	@ghc main.hs -o out