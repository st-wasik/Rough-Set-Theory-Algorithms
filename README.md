# Rough Set Theory Algorithms - University project

This project contains implementatnion of basic Rough Set Theory algorithms. Algorithms are implemented in Haskell.

### How to run
* Install Haskell Platform (from haskell.org).  
* Clone this repo.
* Run following commands, if using cabal-install v2.2.0.0 (tested on Windows):
    * *cabal sandbox init*
    * *cabal update*
    * *cabal install --only-dependencies*
* Or run following commands, if using cabal-install 3.2.0.0 (tested on Ubuntu):
    * *cabal v1-sandbox init*
    * *cabal update*
    * *cabal install --only-dependencies --lib*
* *cabal repl*  
* Then run one from example functions located in *RoughSetTheory.hs*, e.g.  
    * *lem2Example*
    * *classifyExample*
    * *reductExample*
    * *approximationExample*
    * *dataExample*
<br/>
To change approximation level, train or test files, go to *RoughSetTheory.hs* and change defined variables (*trainFile, testFile, approxLevel*).

### License - MIT
