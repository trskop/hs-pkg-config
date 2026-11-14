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

import Data.Bool (not, )
import Data.Function ((.), )
import Data.Functor ((<$>), )
import Data.List qualified as List (filter, )
import Data.Semigroup ((<>), )
import Data.String (fromString, unlines, )
import Data.Version (makeVersion, showVersion, )

import Test.Tasty (TestTree, )
import Test.Tasty.HUnit ((@?=), testCase, )
import Test.Tasty.QuickCheck ((===), (==>), testProperty, )

import Data.PkgConfig
import Data.PkgConfig.Internal.Template (Template(Template), )
import Data.PkgConfig.Internal.Template qualified as Template


-- Notes:
--
-- * Comparison of `Template` is not structural, it converts the templates to
--   (lazy) Text and compares the results. To use structural comparison one
--   needs to unpack it into fragments.
--
-- * Use `Template.empty` instead of `mempty` unless `mempty` itself is tested.
--   The reason is that `mempty` is derived and `Template.empty` is a
--   definition that can be inspected.

tests :: [TestTree]
tests =
    [ testCase "toString empty" do
        toString empty @?= ""

    , testCase "toStrictText empty" do
        toStrictText empty @?= ""

    , testProperty "versionFields" \v ->
        Template.toStrictText (versionFields v)
            === fromString (showVersion (makeVersion v))

    , testProperty "Template.empty </> x ≡ x" \x ->
        let Template actual = Template.empty </> x
            Template expected = x

        in  actual === expected

    , testProperty "x </> empty ≡ x" \x ->
        let Template actual = x </> Template.empty
            Template expected = x

        in  actual === expected

    , testProperty "ext ≠ empty ⇒ empty <.> ext ≡ \".\" <> ext" \ext ->
        let Template actual = Template.empty <.> ext
            Template expected = strLit "." <> ext

        in  not (Template.null ext) ==> actual === expected

    , testProperty "base <.> Template.empty ≡ base" \base ->
        let Template actual = base <.> Template.empty
            Template expected = base

        in  actual === expected

    , testProperty "separatedBy" \sep xs ->
        let xs' = Template <$> xs
        in  separatedBy sep xs'
                === separatedBy sep (List.filter (not . Template.null) xs')

    , testCase "Example (1)" do
        let actual = empty
                { variables =
                   [ "prefix" .= "/usr/local"
                   , "includedir" .= (var "prefix" </> "include")
                   ]
                }

            expected = unlines
                [ "prefix=/usr/local"
                , "includedir=${prefix}/include"
                , ""
                ]

        toString actual @?= expected

    , testCase "Example (2)" do
        let actual = empty
                { name = "some library"
                , description = "Example pkg-config"
                , cflags = includes ["/usr/lib/foo"]
                , libs = options
                    [ libraryPath ["/usr/lib/foo"]
                    , libraries ["foo"]
                    ]
                }

            expected = unlines
                [ "Name: some library"
                , "Description: Example pkg-config"
                , "Cflags: -I\"/usr/lib/foo\""
                , "Libs: -L\"/usr/lib/foo\" -lfoo"
                ]

        toString actual @?= expected

    , testCase "Example (3)" do
        let actual = empty
                { variables =
                   [ "prefix"     .= "/usr/local"
                   , "includedir" .= (var "prefix" </> "include")
                   , "libdir"     .= (var "prefix" </> "lib")
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
