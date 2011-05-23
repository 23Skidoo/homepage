hakyll : hakyll.hs
	ghc --make hakyll.hs

all! : hakyll
	./hakyll rebuild

clean :
	rm -rf hakyll hakyll.o hakyll.hi

clean-all :
	rm -rf hakyll hakyll.o hakyll.hi _cache _site
