.PHONY: tests shell bench pl docs coverage

tests:
	(cd specs; runghc -i../src Specs)

shell:
	ghci -isrc -outputdir dist/build Prolog GraphViz Quote IsString -XOverloadedStrings -XQuasiQuotes

bench:
	( cd bench; \
	  ghc -i../src -O --make Bench -main-is Bench -o runbench && \
	  time -p ./runbench 8 \
	)

pl:
	ghc -isrc -outputdir dist/build -O --make Console -main-is Console -o $@

docs:
	cabal configure && cabal haddock --hyperlink-source

coverage:
	ghc -fhpc -isrc -outputdir dist/build Specs -main-is Specs -o coverage/runspecs
	cd coverage; ./runspecs ../specs 2>/dev/null >/dev/null
	hpc report coverage/runspecs
	hpc markup coverage/runspecs --destdir=coverage --exclude=Prolog --exclude=Specs
	rm coverage/runspecs*
