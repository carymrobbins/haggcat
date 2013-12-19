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
cd cabal
# The official way to install cabal-install.
cabal install Cabal/ cabal-install/
# If you have issues, try the bootstrap method instead.
cd cabal-install
./bootstrap.sh
```

Once you have the latest version of cabal -

```bash
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Testing
-------

To run the tests, create a test-files directory in the root of
this project.  Create files containing consumerKey, consumerSecret,
issuerId, customerId, and certificate.key, which is your X509 private key.

```bash
cabal configure --enable-tests
cabal test
```

