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
