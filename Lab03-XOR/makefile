clean:
	rm *.out ; rm *.o ; rm *.hi

run: out
	@./out $(ARGS)

out: main.hs
	@ghc main.hs -o out