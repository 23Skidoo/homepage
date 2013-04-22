GHC_VERSION=7.4.2

hakyll : hakyll.hs
	ghc -no-user-package-conf -package-conf \
	cabal-dev/packages-$(GHC_VERSION).conf -Wall --make hakyll.hs

all : hakyll
	./hakyll rebuild

clean :
	rm -rf hakyll hakyll.o hakyll.hi

clean-all :
	rm -rf hakyll hakyll.o hakyll.hi _cache _site
