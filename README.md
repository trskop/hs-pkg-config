Hs-pkg-config
=============

[![Hackage](http://img.shields.io/hackage/v/hs-pkg-config.svg)][Hackage: hs-pkg-config]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/hs-pkg-config.svg)](http://packdeps.haskellers.com/reverse/hs-pkg-config)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/hs-pkg-config.svg)](https://travis-ci.org/trskop/hs-pkg-config)


Description
-----------

Library for creating [pkg-config][Pkg-config Homepage] configuration files from
Haskell. [Pkg-config][Pkg-config Homepage] is a tool for inserting correct
compiler options when compiling libraries or applications. It is
language-agnostic, but mainly used for building various C or C++ libraries and
applications.

One of the possible usage examples of this library is generating `.pc` files
from [Shake build system][Shake Homepage].


Example
-------

Following Haskell code is able to generate package configuration file named
`foo.pc` for library `foo`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main)
  where

import Data.String (IsString)

import Data.Default.Class (Default(def))
   -- From data-default-class library:
   -- http://hackage.haskell.org/package/data-default-class

import Control.Lens
   -- From lens library:
   -- http://hackage.haskell.org/package/lens

import Data.PkgConfig


libraryBaseName :: IsString a => a
libraryBaseName = "foo"

main :: IO ()
main = writePkgConfig (libraryBaseName ++ ".pc") libPkgConfig
  where
    libPkgConfig = def
        & pkgVariables   .~
            [ ("prefix",     "/usr/local"              )
            , ("includedir", var "prefix" </> "include")
            , ("libdir",     var "prefix" </> "lib"    )
            , ("arch",       "i386"                    )
            ]
        & pkgName        .~ libraryBaseName
        & pkgDescription .~ "Example pkg-config."
        & pkgVersion     .~ version [1, 2, 3]
        & pkgCflags      .~ includes [var "includedir"]
        & pkgRequires    .~ list
            [ "bar" ~> [0], "bar" ~<= [3, 1]
            , "baz" ~= [1, 2, 3]
            ]
        & pkgLibs        .~ options
            [ libraryPath [var "libdir", var "libdir" </> var "arch"]
            , libraries [libraryBaseName]
            ]
```

Content of `foo.pc`:

```
prefix=/usr/local
includedir=${prefix}/include
libdir=${prefix}/lib
arch=i386

Name: foo
Description: Example pkg-config.
Version: 1.2.3
Requires: bar > 0, bar <= 3.1, baz = 1.2.3
Cflags: -I"${includedir}"
Libs: -L"${libdir}" -L"${libdir}/${arch}" -lfoo
```

Now lets see if `pkg-config` would be able to tell us something about this
library:

    $ PKG_CONFIG_PATH=`pwd` pkg-config --modversion foo
    1.2.3

Note that asking for `--cflags` or other options would result in error saying
that it is unable to find `bar` and `baz` libraries. That is OK, it means that
it is able to parse file correctly, we just hadn't provided `bar.pc` and
`baz.pc`. If we delete lines that define `Requires:` field, then `pkg-config`
would be able to give us `--cflags`, `--libs`, etc. You can try it.


Building options
----------------

* `-fpedantic` (disabled by default)

  Pass additional warning flags including `-Werror` to GHC during compilation.



[Hackage: hs-pkg-config]:
  https://hackage.haskell.org/package/hs-pkg-config
  "Hackage: hs-pkg-config"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[Pkg-config Homepage]:
  http://www.freedesktop.org/wiki/Software/pkg-config/
  "Pkg-config Homepage"
[Shake Homepage]:
  http://shakebuild.com
  "Shake Homepage"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
