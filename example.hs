{-# LANGUAGE OverloadedStrings #-}
module Main (main)
  where

import Data.String (IsString)

import Control.Lens
   -- From lens library:
   -- http://hackage.haskell.org/package/lens

import Data.PkgConfig


libraryBaseName :: IsString a => a
libraryBaseName = "foo"

main :: IO ()
main = writePkgConfig (libraryBaseName ++ ".pc") libPkgConfig
  where
    libPkgConfig = empty
        & pkgVariables   .~
            [ ("prefix",     "/usr/local"              )
            , ("includedir", var "prefix" </> "include")
            , ("libdir",     var "prefix" </> "lib"    )
            , ("arch",       "i386"                    )
            ]
        & pkgName        .~ libraryBaseName
        & pkgDescription .~ "Example pkg-config."
        & pkgVersion     .~ versionWord [1, 2, 3]
        & pkgCflags      .~ includes [var "includedir"]
        & pkgRequires    .~ list
            [ "bar" ~> [0], "bar" ~<= [3, 1]
            , "baz" ~= [1, 2, 3]
            ]
        & pkgLibs        .~ options
            [ libraryPath [var "libdir", var "libdir" </> var "arch"]
            , libraries [libraryBaseName]
            ]
