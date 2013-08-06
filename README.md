# cabal-ghci

This haskell package lets you load options from your `.cabal` file into your ghci session. It offers both a `cabal-ghci` executable as a very simple wrapper around ghci, and the more powerful `:cabal-set` ghci command that can be added to your `.ghci`.

* [Documentation on hackage](http://hackage.haskell.org/packages/archive/cabal-ghci/latest/doc/html/Distribution-Dev-Interactive.html)

In this example, after invoking `:cabalset`, the correct language
extensions and include paths are set.

```
Prelude> :cabalset
:set "-i/home/atnnn/up1/src/dist/build/autogen"
:set "-i/home/atnnn/up1/src/web"
:set "-XUnicodeSyntax"
:set "-Wall"
Prelude> a ← return 1
Prelude> :load Main
Ok, modules loaded: Main, Application, Handlers.
```

This works from any sub-directory of your project and accepts the
following arguments:

* `-fflag` -- enable flag
* `-f-flag`-- disable flag
* an executable name -- loads the options for the executable

To install:

```
$ cabal update
$ cabal install cabal-ghci
$ head -n 4 >> ~/.ghci
:m + Distribution.Dev.Interactive
:def cabalset cabalSet
:cabalset
:m - Distribution.Dev.Interactive
```