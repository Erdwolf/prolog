.PHONY: tests shell bench pl docs

tests:
	(cd specs; runghc -i../src Specs)

shell:
	ghci -isrc -outputdir dist/build Prolog GraphViz Quote -XOverloadedStrings -XQuasiQuotes

bench:
	( cd bench; \
	  ghc -i../src -O --make Bench -main-is Bench -o runbench && \
	  time -p ./runbench 8 \
	)

pl:
	ghc -isrc -outputdir dist/build -O --make Prolog -main-is Prolog -o $@

docs:
	cabal configure && cabal haddock --hyperlink-source
