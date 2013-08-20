all: Outdated MultiVersion Unregister

Outdated: Outdated.hs
	ghc -O --make Outdated
MultiVersion: MultiVersion.hs
	ghc -O --make MultiVersion
Unregister: Unregister.hs
	ghc -O --make Unregister

.PHONY: clean
clean:
	rm -f *.hi *.o Outdated MultiVersion Unregister
