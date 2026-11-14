Hs-pkg-config
=============

[![Hackage](http://img.shields.io/hackage/v/hs-pkg-config.svg)][Hackage: hs-pkg-config]
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


Description
-----------

Library for creating [pkg-config][Pkg-config Homepage] configuration files from
Haskell. [Pkg-config][Pkg-config Homepage] is a tool for inserting correct
compiler options when compiling libraries or applications. It is
language-agnostic, but mainly used for building various C or C++ libraries and
applications.

This library is compatible with [pkgconf][Pkgconf public repository] which is
an alternative implementation of [pkg-config][Pkg-config Homepage] that
provides additional functionality.

This library can be also used to generate [`pc(5)`][pkg-confing file format]
files from [Shake build system][Shake Homepage].


Example
-------

Following Haskell code is able to generate package configuration file named
`foo.pc` for library `foo`:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Main (main)
  where

import Data.String (IsString)
import System.IO (IO)

import System.FilePath (addExtension)
   -- From filepath library:
   -- http://hackage.haskell.org/package/filepath

import Data.PkgConfig


libraryBaseName :: IsString a => a
libraryBaseName = "foo"

main :: IO ()
main = writePkgConfig (libraryBaseName `addExtension` "pc") libPkgConfig
  where
    libPkgConfig = empty
        { variables   =
            [ "prefix"     .= "/usr/local"
            , "includedir" .= (var "prefix" </> "include")
            , "libdir"     .= (var "prefix" </> "lib")
            , "arch"       .= "i386"
            ]
        , name        = libraryBaseName
        , description = "Example pkg-config."
        , version     = versionWord [1, 2, 3]
        , cflags      = includes [var "includedir"]
        , requires    = list ["tinfo" ~> [1, 0]]
        , libs        = options
            [ libraryPath [var "libdir", var "libdir" </> var "arch"]
            , libraries [libraryBaseName]
            ]
        }
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
Requires: tinfo > 1.0
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
  "The Haskell programming language homepage"
[Pkg-config Homepage]:
  http://www.freedesktop.org/wiki/Software/pkg-config/
  "Pkg-config Homepage"
[Pkgconf public repository]:
  https://github.com/pkgconf/pkgconf
  "pkgconf public repository"
[Shake Homepage]:
  http://shakebuild.com
  "Shake build system homepage"
[pkg-confing file format]:
  https://man.archlinux.org/man/pc.5.en
  "pc(5) manual page describing pkg-config file format"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
