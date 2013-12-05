GHC_VERSION=i386-linux-ghc-7.6.3

hakyll : hakyll.hs
	ghc -no-user-package-db -package-db \
	.cabal-sandbox/$(GHC_VERSION)-packages.conf.d -Wall --make hakyll.hs \
	-odir dist -hidir dist -o $@

all : hakyll
	./hakyll rebuild

clean :
	rm -rf hakyll hakyll.o hakyll.hi

clean-all :
	rm -rf hakyll hakyll.o hakyll.hi _cache _site
