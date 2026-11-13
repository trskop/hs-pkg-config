-- |
-- Module:      Tests.Data.PkgConfig
-- Description: Unit tests for Data.PkgConfig module
-- Copyright:   (c) 2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- __TL;DR__ Unit tests for "Data.PkgConfig" module.
module Tests.Data.PkgConfig
    ( tests
    )
  where

import Data.String (fromString, unlines, )
import Data.Version (makeVersion, showVersion, )

import Test.Tasty (TestTree, )
import Test.Tasty.HUnit ((@?=), testCase, )
import Test.Tasty.QuickCheck ((===), testProperty, )

import Data.PkgConfig
import Data.PkgConfig.Internal.Template qualified as Template


tests :: [TestTree]
tests =
    [ testCase "toString empty" do
        toString empty @?= "\n"

    , testProperty "versionFields" \v ->
        Template.toStrictText (versionFields v)
            === fromString (showVersion (makeVersion v))

    , testCase "example 1" do
        let actual = empty
                { variables =
                   [ "prefix" .= "/usr/local"
                   , "includedir" .= var "prefix" </> "include"
                   ]
                }

            expected = unlines
                [ "prefix=/usr/local"
                , "includedir=${prefix}/include"
                , ""
                ]

        toString actual @?= expected

    , testCase "example 2" do
        let actual = empty
                { variables =
                   [ "prefix"     .= "/usr/local"
                   , "includedir" .= var "prefix" </> "include"
                   , "libdir"     .= var "prefix" </> "lib"
                   , "arch"       .= "i386"
                   ]
                , name = "some library"
                , description = "Example pkg-config"
                , cflags = includes [var "includedir" </> "foo"]
                , libs = options
                    [ libraryPath [var "libdir" </> var "arch"]
                    , libraries ["foo"]
                    ]
                }

            expected = unlines
                [ "prefix=/usr/local"
                , "includedir=${prefix}/include"
                , "libdir=${prefix}/lib"
                , "arch=i386"
                , ""
                , "Name: some library"
                , "Description: Example pkg-config"
                , "Cflags: -I\"${includedir}/foo\""
                , "Libs: -L\"${libdir}/${arch}\" -lfoo"
                ]

        toString actual @?= expected

    ]
