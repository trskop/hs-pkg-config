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

import Data.Bool (Bool(False, True), not, )
--import Data.Function ((.), )
--import Data.List qualified as List (concatMap, )
import Data.Monoid (mempty, )
import Data.Semigroup ((<>), )
import Data.String (fromString, )

import Data.Text qualified as Strict.Text
import Test.QuickCheck.Instances.Text () -- Instances only
import Test.Tasty (TestTree, )
import Test.Tasty.HUnit ((@?=), assertBool, testCase, )
import Test.Tasty.QuickCheck ((.&&.), (===), (==>), testProperty, )

import Data.PkgConfig.Internal.Template


tests :: [TestTree]
tests =
    [ testCase "Eq" do
        Template [Literal "foo", Literal "bar"] @?= Template [Literal "foobar"]
        Template [Variable "foo", Variable "bar"]
            @?= Template [Variable "foo", Variable "bar"]
        Template [Literal "foo", Variable "bar"]
            @?= Template [Literal "foo", Variable "bar"]
        Template [Variable "foo", Literal "bar"]
            @?= Template [Variable "foo", Literal "bar"]

    , testCase "toString empty" do
        toStrictText empty @?= ""

    , testCase "mempty ≡ empty" do
        mempty @?= empty
        fragments mempty @?= fragments empty

    , testCase "null empty" do
        assertBool "'null (Template [])' failed to return 'True'"
            (null (Template []))
        assertBool "'null empty' failed to return 'True'" (null empty)

    , testProperty "not (null x); where x ≠ empty" \x ->
        -- This is to avoid using 'empty' and 'null' in a circular reference:
        let precondition = \case
                Template [] -> False
                _           -> True

        in  precondition x ==> not (null x)

    , testCase "lit \"\" = empty" do
        fragments (lit "") @?= fragments empty

    , testProperty "lit t = Template [Listeral t]" \t ->
        not (Strict.Text.null t) ==> fragments (lit t) === [Literal t]

    , testCase "lit escapes properly" do
        toStrictText (lit "${prefix}") @?= "$${prefix}"
        toStrictText (lit "foo\\bar") @?= "foo\\\\bar"
        toStrictText (lit "# foo bar") @?= "\\# foo bar"

    , testProperty "fromString ≡ strLit" \x ->
        fragments (fromString x) === fragments (strLit x)

    , testProperty "strLit s = lit (fromString s)" \s ->
        fragments (strLit s) === fragments (lit (fromString s))

    , testCase "var \"\" = empty" do
        fragments (var "") @?= fragments empty

    , testProperty "var t = Template [Variable t]" \t ->
        not (Strict.Text.null t) ==> fragments (var t) === [Variable t]

    , testProperty "strVar s = var (fromString s)" \s ->
        fragments (strVar s) === fragments (var (fromString s))

    , testProperty "singletonLit c = fromString [c]" \c ->
        fragments (singletonLit c) === fragments (fromString [c])

    , testProperty "Semigroup associativity law" \x y z ->
        fragments (x <> (y <> z)) === fragments ((x <> y) <> z)

    , testProperty "Monoid identity laws" \x ->
        {- HLINT ignore "Monoid law, left identity" -}
        {- HLINT ignore "Monoid law, right identity" -}
        fragments (x <> mempty) === fragments x
            .&&. fragments (mempty <> x) === fragments x

    , testCase "getVariables" do
        getVariables empty @?= []
        getVariables (Template [Variable "foo"]) @?= ["foo"]
        getVariables (Template [Literal "x", Variable "foo"]) @?= ["foo"]
        getVariables (Template [Variable "foo", Variable "bar"])
            @?= ["foo", "bar"]

    ]
