{-# LANGUAGE OverloadedLabels #-}
-- |
-- Module:      Tests.Data.PkgConfig.Validate
-- Description: Unit tests for Data.PkgConfig.Validate module
-- Copyright:   (c) 2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- __TL;DR__ Unit tests for "Data.PkgConfig.Validate" module.
module Tests.Data.PkgConfig.Validate
    ( tests
    )
  where

import Data.Bifunctor (first, )
import Data.List.NonEmpty (NonEmpty((:|)), )
import Data.List.NonEmpty qualified as NonEmpty (sort, )

import Data.Either.Validation (Validation(Failure), )
import Test.Tasty (TestTree, )
import Test.Tasty.HUnit ((@?=), testCase, )

import Data.PkgConfig
import Data.PkgConfig.Validate


tests :: [TestTree]
tests =
    [ testCase "validate empty" do
        first NonEmpty.sort (validate empty) @?= Failure
            (     MissingRequiredFieldError #description
            :|  [ MissingRequiredFieldError #name
                , MissingRequiredFieldError #url
                , MissingRequiredFieldError #version
                ]
            )

    , testCase "validate example1" do
        let example1 = empty
                { variables = ["foo" .= (var "prefix" </> "bar")]
                , cflags = var "prefix"
                }

        first NonEmpty.sort (validate example1) @?= Failure
            (     MissingRequiredFieldError #description
            :|  [ MissingRequiredFieldError #name
                , MissingRequiredFieldError #url
                , MissingRequiredFieldError #version
                , UndefinedVariableError #cflags "prefix" ""
                , UndefinedVariableError #variables "prefix" "foo"
                , UnusedVariableError "foo"
                ]
            )

    , testCase "validate example2" do
        let example1 = empty
                { cflags = includes
                    [ var "includedir"
                    , var "includedir" </> var "platform"
                    ]
                , cflagsPrivate = ""
                , cflagsShared = ""
                , conflicts = ""
                , copyright = "(c) Maintainer"
                , description = ""
                , libs = options
                    [ libraryPath
                        [ var "libdir"
                        , var "libdir" </> var "platform"
                        ]
                    , libraries ["tinfo"]
                    ]
                , libsPrivate = ""
                , libsShared = ""
                , license = "BSD-3-Clause"
                , licenseFile = ""
                , linkAbi = ""
                , maintainer = "maintainer@example.tld"
                , name = "foo"
                , provides = "foo"
                , requires = list
                    [ "tinfo" ~> [1, 0]
                    ]
                , requiresInternal = ""
                , requiresPrivate = ""
                , requiresShared = ""
                , source = ""
                , url = "https://www.example.tld/foo"
                , variables =
                    [ "prefix" .= "/usr"
                    , "includedir" .= (var "prefix" </> "include")
                    , "libdir" .= (var "prefix" </> "lib")
                    , "platform" .= "x86_64-linux-gnu"
                    , "datadir" .= (var "prefix" </> "share/foo")
                    ]
                , version = "1.2.3"
                }

        first NonEmpty.sort (validate example1) @?= Failure
            (     MissingRequiredFieldError #description
            :|  [ UnusedVariableError "datadir"
                ]
            )

    ]
