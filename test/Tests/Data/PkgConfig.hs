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
import Data.Char qualified as Char (isAlphaNum, )
import Data.Function (($), (&), (.), )
import Data.Functor.Identity (Identity(Identity, runIdentity), )
import Data.Int (Int, )
import Data.List qualified as List (filter, )
import Data.Monoid (mempty, )
import Data.Semigroup ((<>), )
import Data.String (fromString, unlines, )
import Data.Version (makeVersion, showVersion, )
import Data.Word (Word, )
import Text.Show (show, )

import Data.Text qualified as Strict.Text (singleton)
import Test.Tasty (TestTree, testGroup, )
import Test.Tasty.HUnit ((@?=), testCase, )
import Test.Tasty.QuickCheck
    ( (===)
    , (==>)
    , arbitraryASCIIChar
    , forAll
    , testProperty
    , )

import Data.PkgConfig
import Data.PkgConfig.Internal.Template (Template(fragments), )
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

    , testProperty "fromString . toString = toStrictText" \x ->
        fromString (toString x) === toStrictText x

    , testGroup "File path combinators"
        [ testProperty "Template.empty </> x ≡ x" \x ->
            fragments (Template.empty </> x) === fragments x

        , testProperty "x </> empty ≡ x" \x ->
            fragments (x </> Template.empty) === fragments x

        , testProperty "ext ≠ empty ⇒ empty <.> ext ≡ \".\" <> ext" \ext ->
            not (Template.null ext)
                ==> fragments (Template.empty <.> ext)
                    === fragments (strLit "." <> ext)

        , testProperty "base <.> Template.empty ≡ base" \base ->
            fragments (base <.> Template.empty) === fragments base

        ]

    , testProperty "separatedBy" \sep ts ->
        separatedBy sep ts
            === separatedBy sep (List.filter (not . Template.null) ts)

    , testProperty "list" \ts ->
        list ts === separatedBy ", " (List.filter (not . Template.null) ts)

    , testProperty "options" \ts ->
        options ts === separatedBy " " (List.filter (not . Template.null) ts)

    , testProperty "option empty \"<s>\" ~ \"\\\"<s>\\\"\"" \s ->
        Template.toStrictText (option "" s)
            === Strict.Text.singleton '"' <> Template.toStrictText s
                <> Strict.Text.singleton '"'

    , testProperty "option \"-<c>\" empty ~ \"-<c>\\\"\\\"\""
        $ forAll arbitraryASCIIChar \c ->
            let opt = Strict.Text.singleton '-' <> Strict.Text.singleton c

            in  Char.isAlphaNum c
                    ==> Template.toStrictText (option opt Template.empty)
                        === opt <> "\"\""

    , testProperty "option \"-<c>\" \"<s>\" ~ \"-<c>\\\"<s>\\\"\""
        $ forAll arbitraryASCIIChar \c s ->
            let opt = Strict.Text.singleton '-' <> Strict.Text.singleton c

                expected = opt <> Strict.Text.singleton '"'
                    <> Template.toStrictText s
                    <> Strict.Text.singleton '"'

            in  Char.isAlphaNum c
                    ==> Template.toStrictText (option opt s) === expected

    , testCase "includes" do
        fragments (includes []) @?= []
        fragments (includes [""]) @?= []
        fragments (includes ["", mempty]) @?= []

        let incdir1 = var "prefix" </> "include"
            incdir2 = incdir1 </> "x86_64"
        fragments (includes [incdir1, "", incdir2, mempty])
            @?= fragments (includes [incdir1, incdir2])

    , testCase "libraries" do
        fragments (libraries []) @?= []
        fragments (libraries [""]) @?= []
        fragments (libraries ["", mempty]) @?= []

        fragments (libraries ["m", "", "tinfo", mempty])
            @?= fragments (libraries ["m", "tinfo"])

    , testCase "libraryPath" do
        fragments (libraryPath []) @?= []
        fragments (libraryPath [""]) @?= []
        fragments (libraryPath ["", mempty]) @?= []

        let libdir1 = var "prefix" </> "lib"
            libdir2 = libdir1 </> "x86_64"

        fragments (libraryPath [libdir1, "", libdir2, mempty])
            @?= fragments (libraryPath [libdir1, libdir2])

    , testGroup "Version Combinators"
        [ testProperty "versionField @Word" \n ->
            Template.toStrictText (versionField @Word n) === fromString (show n)

        , testProperty "versionField @Int" \n ->
            Template.toStrictText (versionField @Int n) === fromString (show n)

        , testProperty "versionFields @Int ~ makeVersion" \v ->
            Template.toStrictText (versionFields v)
                === fromString (showVersion (makeVersion v))

        , testProperty "mempty ~= v = mempty" \vs ->
            fragments ("" ~= vs) === fragments Template.empty

        , testProperty "pkg ~= [] = pkg" \pkg ->
            fragments (pkg ~= []) === fragments (lit pkg)

        , testCase "Usage samples for (~=)" do
            Template.toStrictText ("pkg" ~= [1, 2, 3]) @?= "pkg = 1.2.3"

        , testProperty "mempty ~/= v = mempty" \vs ->
            fragments ("" ~/= vs) === fragments Template.empty

        , testProperty "pkg ~/= [] = pkg" \pkg ->
            fragments (pkg ~/= []) === fragments (lit pkg)

        , testCase "Usage samples for (~/=)" do
            Template.toStrictText ("pkg" ~/= [1, 2, 3]) @?= "pkg != 1.2.3"

        , testProperty "mempty ~< v = mempty" \vs ->
            fragments ("" ~< vs) === fragments Template.empty

        , testProperty "pkg ~< [] = pkg" \pkg ->
            fragments (pkg ~< []) === fragments (lit pkg)

        , testCase "Usage samples for (~<)" do
            Template.toStrictText ("pkg" ~< [1, 2, 3]) @?= "pkg < 1.2.3"

        , testProperty "mempty ~<= v = mempty" \vs ->
            fragments ("" ~<= vs) === fragments Template.empty

        , testProperty "pkg ~<= [] = pkg" \pkg ->
            fragments (pkg ~<= []) === fragments (lit pkg)

        , testCase "Usage samples for (~<=)" do
            Template.toStrictText ("pkg" ~<= [1, 2, 3]) @?= "pkg <= 1.2.3"

        , testProperty "mempty ~> v = mempty" \vs ->
            fragments ("" ~> vs) === fragments Template.empty

        , testProperty "pkg ~> [] = pkg" \pkg ->
            fragments (pkg ~> []) === fragments (lit pkg)

        , testCase "Usage samples for (~>)" do
            Template.toStrictText ("pkg" ~> [1, 2, 3]) @?= "pkg > 1.2.3"

        , testProperty "mempty ~>= v = mempty" \vs ->
            fragments ("" ~>= vs) === fragments Template.empty

        , testProperty "pkg ~>= [] = pkg" \pkg ->
            fragments (pkg ~>= []) === fragments (lit pkg)

        , testCase "Usage samples for (~>=)" do
            Template.toStrictText ("pkg" ~>= [1, 2, 3]) @?= "pkg >= 1.2.3"

        ]

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

    , testCase "Example (4)" do
        let actual = empty
                & pkgVariables .~
                    [ "prefix"     .= "/usr/local"
                    , "includedir" .= (var "prefix" </> "include")
                    , "libdir"     .= (var "prefix" </> "lib")
                    , "arch"       .= "i386"
                    ]
                & pkgName .~ "some library"
                & pkgDescription .~ "Example pkg-config"
                & pkgCflags .~ includes [var "includedir" </> "foo"]
                & pkgLibs .~ options
                    [ libraryPath [var "libdir" </> var "arch"]
                    , libraries ["foo"]
                    ]

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

-- So that a whole library doesn't need to imported.
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
l .~ b = runIdentity . l (\_ -> Identity b)
infixr 4 .~
