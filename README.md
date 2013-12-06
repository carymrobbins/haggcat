Haggcat
=======

A Haskell client for Intuit's Customer Account Data API.

***This library is in its early stages and is not ready for use.***

Building
--------

Be sure to use cabal sandbox.  If you don't already have it, install it from
the latest version of cabal.

```bash
cd /where/you/want/to/clone
git clone git://github.com/haskell/cabal.git
cd cabal/cabal-install
./bootstrap.sh
```

Once you have the latest version of cabal -

```bash
cabal sandbox init
cabal install --only-dependencies
cabal build
```

