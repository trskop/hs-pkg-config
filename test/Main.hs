-- |
-- Module:      Main
-- Description: Unit tests for hs-pkg-config
-- Copyright:   (c) 2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- __TL;DR__ Unit tests for @hs-pkg-config@.
module Main
    ( main
    )
  where

import System.IO (IO, )

import Test.Tasty (TestTree, defaultMain, testGroup, )

import Tests.Data.PkgConfig qualified (tests, )
import Tests.Data.PkgConfig.Internal.Template qualified (tests, )
import Tests.Data.PkgConfig.Validate qualified (tests, )


main :: IO ()
main = defaultMain (testGroup "Tests.Data" tests)

tests :: [TestTree]
tests =
    [ testGroup "PkgConfig"
        Tests.Data.PkgConfig.tests
    , testGroup "PkgConfig.Internal.Template"
        Tests.Data.PkgConfig.Internal.Template.tests
    , testGroup "PkgConfig.Validate"
        Tests.Data.PkgConfig.Validate.tests
    ]
