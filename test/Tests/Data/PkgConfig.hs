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
                   [ ("prefix", "/usr/local")
                   , ("includedir", var "prefix" </> "include")
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
                   [ ("prefix", "/usr/local")
                   , ("includedir", var "prefix" </> "include")
                   ]
                , name = "some library"
                , libs = includes
                    [ var "includedir" </> "foo"
                    ]
                }

            expected = unlines
                [ "prefix=/usr/local"
                , "includedir=${prefix}/include"
                , ""
                , "Name: some library"
                , "Libs: -I\"${includedir}/foo\""
                ]

        toString actual @?= expected

    ]
