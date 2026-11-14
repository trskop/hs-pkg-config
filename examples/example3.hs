{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Main (main)
  where

import Data.String (IsString)
import System.IO (IO)

import Control.Lens hiding ((.=))
   -- From lens library:
   -- http://hackage.haskell.org/package/lens
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
        & pkgVariables   .~
            [ "prefix"     .= "/usr/local"
            , "includedir" .= (var "prefix" </> "include")
            , "libdir"     .= (var "prefix" </> "lib")
            , "arch"       .= "i386"
            ]
        & pkgName        .~ libraryBaseName
        & pkgDescription .~ "Example pkg-config."
        & pkgVersion     .~ versionWord [1, 2, 3]
        & pkgCflags      .~ includes [var "includedir"]
        & pkgRequires    .~ list ["tinfo" ~> [1]]
        & pkgLibs        .~ options
            [ libraryPath [var "libdir", var "libdir" </> var "arch"]
            , libraries [libraryBaseName]
            ]
