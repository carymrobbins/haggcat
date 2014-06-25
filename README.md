Haggcat
=======

A Haskell client for Intuit's Customer Account Data API.

***This library is in its early stages and is not ready for use.***

Building
--------

If you are running Ubuntu you will need to install the following packages -

```bash
sudo apt-get install libcurl4-openssl-dev
```

To build in a sandbox -

```bash
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Testing
-------

To run the tests, create a test-files directory in the root of
this project.  You will need two files: config and certificate.key, which is your X509 private key.
Config should contain something along the lines of -

```haskell
Config { consumerKey    = "xxxxxxxxx"
       , consumerSecret = "yyyyyyyyy"
       , issuerId = "myapp.12345.intuit.ipp.prod"
       , customerId = "0"
       , privateKeyPath = "test-files/certificate.key"
       }
```

You can then install the test dependencies and run the test suite via -

```bash
cabal install --enable-tests --dependencies-only
cabal test
```
