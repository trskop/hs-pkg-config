-- |
-- Module:      Tests.Data.PkgConfig.Internal.Template
-- Description: Unit tests for Data.PkgConfig module
-- Copyright:   (c) 2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- __TL;DR__ Unit tests for "Data.PkgConfig" module.
module Tests.Data.PkgConfig.Internal.Template
    ( tests
    )
  where

import Data.Bool (not, )
--import Data.Function ((.), )
--import Data.List qualified as List (concatMap, )
import Data.Monoid (mempty, )
import Data.Semigroup ((<>), )
import Data.String (fromString, )

import Data.Text qualified as Strict.Text
import Test.Tasty (TestTree, testGroup, )
import Test.Tasty.HUnit ((@?=), testCase, )
import Test.Tasty.QuickCheck ((===), (==>), testProperty, )

import Data.PkgConfig.Internal.Template


tests :: [TestTree]
tests =
    [ testCase "toString empty" do
        toStrictText empty @?= ""

    , testCase "mempty ≡ empty" do
        mempty @?= empty

        let Template actual = mempty
            Template expected = empty

        actual @?= expected

    , testProperty "fromString ≡ strLit" \x ->
        fromString x === strLit x

    , testCase "lit \"\" = empty" do
        lit "" @?= empty

    , testProperty "lit t = Template [Listeral t]" \s ->
        let t = fromString s
            Template actual = lit t

        in  not (Strict.Text.null t) ==> actual === [Literal t]

    , testCase "lit escapes properly" do
        toStrictText (lit "${prefix}") @?= "$${prefix}"
        toStrictText (lit "foo\\bar") @?= "foo\\\\bar"
        toStrictText (lit "# foo bar") @?= "\\# foo bar"

    , testCase "var \"\" = empty" do
        var "" @?= empty

    , testProperty "var t = Template [Variable t]" \s ->
        let t = fromString s
            Template actual = var t

        in  not (Strict.Text.null t) ==> actual === [Variable t]

    , testGroup "Semigroup"
        [ testProperty "associativity (1)" \x y z ->
            let Template lhs = strLit x <> (strLit y <> strLit z)
                Template rhs = (strLit x <> strLit y) <> strLit z

            in  lhs === rhs

        , testProperty "associativity (2)" \x y z ->
            let Template lhs = strVar x <> (strVar y <> strVar z)
                Template rhs = (strVar x <> strVar y) <> strVar z

            in  lhs === rhs

        ]

    ]
