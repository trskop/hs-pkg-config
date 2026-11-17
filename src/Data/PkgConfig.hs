-- |
-- Module:       $HEADER$
-- Description:  Create pkg-config configuration files
-- Copyright:    (c) 2014, 2017, 2026 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- __TL;DR__ Create /pkg-config/ configuration files from Haskell code.
module Data.PkgConfig
    (
    -- $top-level

    -- * See also
    -- $see-also

    -- * Usage
    -- $usage

    -- * PkgConfig
    -- $pkg-config
      PkgConfig(..)
    , PkgDescription
    , PkgName
    , PkgUrl
    , PkgVariable
    , PkgVersion
    , empty

    -- ** Lenses
    , pkgVariables
    , pkgName
    , pkgDescription
    , pkgUrl
    , pkgVersion
    , pkgRequires
    , pkgRequiresPrivate
    , pkgConflicts
    , pkgCflags
    , pkgLibs
    , pkgLibsPrivate

    -- ** Serialization
    , toStrictText
    , toString

    -- ** I/O
    , writePkgConfig
    , hPutPkgConfig

    -- * PkgTemplate
    -- $pkg-template
    , PkgTemplate

    -- ** Smart Constructors
    , var
    , lit
    , strLit
    , singletonLit

    -- ** Combinators
    , quote
    , showLit
    , litWith
    , varWith
    , (.=)

    -- ** FilePath-like Combinators
    -- $file-path-combinators
    , (</>)
    , (<.>)

    -- ** Version Combinators
    , VersionField(..)
    , ver
    , versionWord
    , versionInt
    , (~=)
    , (~/=)
    , (~<)
    , (~>)
    , (~<=)
    , (~>=)

    -- ** Options Combinators
    , option
    , strOption
    , includes
    , libraries
    , libraryPath

    -- ** Specialized Folds
    , list
    , options
    , separatedBy

    -- ** Queries
    , getVariables
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Int (Int, Int32, Int64, Int8)
import Data.List qualified as List (concatMap, null)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word, Word32, Word64, Word8)
import System.IO (FilePath, Handle)
import Text.Show (Show(show))

import Data.Text qualified as Strict (Text)
import Data.Text qualified as Strict.Text (null, pack, singleton, unpack)
import Data.Text.IO qualified as Strict.Text (hPutStr, writeFile)

import Data.PkgConfig.Internal.Template
    ( PkgTemplate
    , Template(Template)
    , getVariables
    , lit
    , singletonLit
    , strLit
    , var
    )
import Data.PkgConfig.Internal.Template qualified as Template (empty, null)
import Data.PkgConfig.Internal.PkgConfig
    ( PkgConfig
        ( PkgConfig
        , cflags
        , cflagsPrivate
        , conflicts
        , description
        , libs
        , libsPrivate
        , name
        , provides
        , requires
        , requiresPrivate
        , url
        , variables
        , version
        )
    , PkgDescription
    , PkgName
    , PkgUrl
    , PkgVariable
    , PkgVersion
    , empty
    , pkgCflags
    , pkgConflicts
    , pkgDescription
    , pkgLibs
    , pkgLibsPrivate
    , pkgName
    , pkgRequires
    , pkgRequiresPrivate
    , pkgUrl
    , pkgVariables
    , pkgVersion
    , toStrictText
    )


-- $top-level
--
-- #top-level#
--
-- Create [/pkg-config/](https://www.freedesktop.org/wiki/Software/pkg-config/)
-- configuration files from Haskell code using combinators specialized for this
-- purpose.
--
-- For short overview of what /pkg-config/ is, please, see [pkg-config page on
-- Wikipedia](https://en.wikipedia.org/wiki/Pkg-config).
--
-- To learn more details about /pkg-config/ and its configuration files,
-- please, read one or both following articles:
--
-- * [Guide to pkg-config
--   ](http://people.freedesktop.org/~dbn/pkg-config-guide.html) by Dan
--   Nicholson
--
-- * [Autotools Mythbuster](https://autotools.io/) by Diego Elio Pettenò:
--   [Chapter 4. Dependency discovery -- pkg-config
--   ](https://autotools.io/pkgconfig/index.html)
--
-- This library also supports [/pkgconf/](http://pkgconf.org/), an alternative
-- implementation of /pkg-config/. Be aware that while it maintains
-- compatibility with /pkg-config/ it also provides additional functionality.

-- $see-also
--
-- #see-also#
--
-- * [@pc(5)@](https://man.archlinux.org/man/pc.5.en) manual page documenting
--   /pkg-config/ file format as recognised by /pkgconf/.
--
-- * [@pkgconf(1)@](https://man.archlinux.org/man/pkgconf.1.en) manual page
--   documenting /pkgconf/ command line interface.
--
-- * [/pkgconf/ public repository](https://github.com/pkgconf/pkgconf)
--
-- * [/u-config/ ("micro-config")
--   ](https://github.com/skeeto/u-config), an alternative implementation of
--   /pkg-config/. It has a first class support for Windows, which exposes
--   various corner cases of /pkg-config/ design.

-- {{{ PkgTemplate Combinators ------------------------------------------------

-- | Put quotation marks (\'\"\') around a template. Escaping is not performed.
--
-- >>> quote $ var "prefix" </> "include"
-- "\"${prefix}/include\""
--
-- >>> var "prefix" </> quote "dir with spaces"
-- "${prefix}/\"dir with spaces\""
quote :: PkgTemplate -> PkgTemplate
quote t = singletonLit '"' <> t <> singletonLit '"'

-- | Use `Show` instance to construct a literal.
--
-- @since 0.3.0.0
showLit :: Show a => a -> PkgTemplate
showLit = strLit . show

-- | Constract a literal using provided function.
--
-- @
-- litWith `Data.String.fromString` :: `String` -> `PkgTemplate`
-- litWith (`Data.String.fromString` `.` `show`) :: `Show` a => a -> `PkgTemplate`
-- @
--
-- @since 0.3.0.0
litWith :: (a -> Strict.Text) -> a -> PkgTemplate
litWith f = lit . f

-- | Constract a variable using provided function.
--
-- @
-- varWith `Data.String.fromString` :: `String` -> `PkgTemplate`
-- varWith (`Data.String.fromString` `.` `show`) :: `Show` a => a -> `PkgTemplate`
-- @
--
-- @since 0.3.0.0
varWith :: (a -> Strict.Text) -> a -> PkgTemplate
varWith f = lit . f

-- | Variable definition.
--
-- @
-- \"prefix\" `.=` \"\/usr\/local\"
-- \"includedir\" `.=` (`var` \"prefix\" `</>` \"include\")
-- @
--
-- @since 0.3.0.0
(.=) :: Strict.Text -> PkgTemplate -> PkgVariable
(.=) = (,)
infixr 8 .=

-- {{{ File Path Combinators --------------------------------------------------

-- $file-path-combinators
--
-- #file-path-combinators#
--
-- Cobinators inspired by the [@filepath@ library
-- ](https://hackage.haskell.org/package/filepath).

-- | Put literal \"/\" between two templates.
--
-- >>> var "prefix" </> lit "foo" <.> lit "pc"
-- "${prefix}/foo.pc"
--
-- When either of the arguments is empty are as follows:
--
-- >>> mempty </> lit "foo"
-- "foo"
-- >>> var "prefix" </> mempty
-- "${prefix}"
(</>) :: PkgTemplate -> PkgTemplate -> PkgTemplate
t1 </> t2
  | Template.null t1 = t2
  | Template.null t2 = t1
  | otherwise        = t1 <> singletonLit '/' <> t2
infixr 5 </>

-- | Put literal \".\" between two templates.
--
-- >>> var "prefix" </> lit "foo" <.> lit "pc"
-- "${prefix}/foo.pc"
-- >>> var "major" <.> var "minor" <.> var "patch"
-- "${major}.${minor}.${patch}"
--
-- When either of the arguments is empty are as follows:
--
-- >>> strLit "file" <.> mempty
-- "file"
-- >>> mempty <.> strLit "pc"
-- ".pc"
(<.>) :: PkgTemplate -> PkgTemplate -> PkgTemplate
t1 <.> t2
  | Template.null t2 = t1
  | otherwise        = t1 <> singletonLit '.' <> t2
infixr 7 <.>

-- }}} File Path Combinators --------------------------------------------------

-- {{{ Version Combinators ----------------------------------------------------

-- | Represent a version field as a `PkgTemplate` value.
--
-- @since 0.3.0.0
class VersionField a where
    -- | Create a literal from a version field.
    --
    -- >>> versionField (0 :: Word)
    -- 0
    --
    -- @since 0.3.0.0
    versionField :: a -> PkgTemplate

    -- | Treat list of version fields as version number and construct template
    -- literal out of it.
    --
    -- >>> ver [1, 2, 3 :: Word]
    -- "1.2.3"
    -- >>> versionFields ([] :: [Word]) == mempty
    -- True
    --
    -- >>> :set -XTypeApplications
    -- >>> versionFields @Word [1, 2, 3]
    -- "1.2.3"
    -- >>> versionFields @Word [] == mempty
    -- True
    --
    -- >>> versionFields . versionBranch $ makeVersion [0, 1, 2]
    -- "0.1.2"
    --
    -- >>> :set -XOverloadedRecordDot
    -- >>> import Data.Version
    -- >>> let v = makeVersion [1, 2, 3] in versionFields v.versionBranch
    -- "1.2.3"
    --
    -- @since 0.3.0.0
    versionFields :: [a] -> PkgTemplate
    versionFields = \case
        []     -> Template.empty
        v : vs -> case vs of
            [] -> versionField v
            _  -> versionField v <.> versionFields vs

-- | __INTERNAL__ It's purpose is to simplify derivation of instances.
--
-- @since 0.3.0.0
newtype Shown a = Shown {unShown :: a}

instance Show a => VersionField (Shown a) where
    versionField :: Shown a -> PkgTemplate
    versionField = strLit . show . unShown

deriving via Shown Word   instance VersionField Word
deriving via Shown Word8  instance VersionField Word8
deriving via Shown Word32 instance VersionField Word32
deriving via Shown Word64 instance VersionField Word64

deriving via Shown Int   instance VersionField Int
deriving via Shown Int8  instance VersionField Int8
deriving via Shown Int32 instance VersionField Int32
deriving via Shown Int64 instance VersionField Int64

-- | Treat list of version fields as version number and construct template
-- literal out of it.
--
-- This is a more convenient alias for `versionFields`, see its documentation
-- for more details as well as examples.
--
-- @since 0.3.0.0
ver :: VersionField a => [a] -> PkgTemplate
ver = versionFields

-- | Specialised variant of `ver`.
--
-- >>> versionWord [1, 2, 3]
-- "1.2.3"
--
-- It's also possible to use `ver` directly instead:
--
-- >>> ver [1, 2, 3 :: Word]
-- "1.2.3"
--
-- >>> :set -XTypeApplications
-- >>> ver @Word [1, 2, 3]
-- "1.2.3"
--
-- === Notes:
--
-- Prior to version 0.3.0.0 this function was named @version@, however, that
-- now conflicts with the `PkgConfig` field of the same name.
--
-- @since 0.3.0.0
versionWord :: [Word] -> PkgTemplate
versionWord = ver

-- | Specialised variant of `ver`.
--
-- >>> versionInt [1, 2, 3]
-- "1.2.3"
--
-- It's also possible to use `ver` directly instead:
--
-- >>> ver [1, 2, 3 :: Int]
-- "1.2.3"
--
-- >>> :set -XTypeApplications
-- >>> ver @Int [1, 2, 3]
-- "1.2.3"
versionInt :: [Int] -> PkgTemplate
versionInt = ver

-- | Dependency on a package of exact version.
--
-- >>> "sqlite" ~= [3, 8, 7, 1]
-- "sqlite = 3.8.7.1"
--
-- >>> list ["sqlite" ~= [3, 8, 7, 1], "alpha" ~= [7, 2]]
-- "sqlite = 3.8.7.1, alpha = 7.2"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~=` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~=` [] = pkg
-- @
(~=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~= v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " = " <> versionFields v

-- | Dependency on a package not of a specific version.
--
-- >>> "alpha" ~/= [7, 2]
-- "alpha != 7.2"
--
-- >>> list ["sqlite" ~/= [3, 8, 7, 1], "alpha" ~/= [7, 2]]
-- "sqlite != 3.8.7.1, alpha != 7.2"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~/=` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~/=` [] = pkg
-- @
(~/=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~/= v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " != " <> versionFields v

-- | Dependency on a package with version greater or less then specified
-- value.
--
-- >>> "alpha" ~< [7, 3]
-- "alpha < 7.3"
--
-- >>> list ["sqlite" ~< [3, 9], "alpha" ~< [7, 3]]
-- "sqlite < 3.9, alpha < 7.3"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~<` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~<` [] = pkg
-- @
(~<) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~< v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " < " <> versionFields v

-- | Dependency on a package with version greater then specified value.
--
-- >>> "sqlite" ~> [3, 8]
-- "sqlite3 > 3.8"
--
-- >>> list ["sqlite" ~> [3, 8], "alpha" ~> [7, 1]]
-- "sqlite > 3.8, alpha > 7.1"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~>` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~>` [] = pkg
-- @
(~>) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~> v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " > " <> versionFields v

-- | Dependency on a package with version greater or less or equal then
-- specified value.
--
-- >>> "sqlite" ~<= [4]
-- "sqlite3 <= 4"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~<=` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~<=` [] = pkg
-- @
(~<=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~<= v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " <= " <> versionFields v

-- | Dependency on a package with version greater or equal then specified
-- value.
--
-- >>> "sqlite" ~>= [3, 8]
-- "sqlite3 >= 3.8"
--
-- === Properties:
--
-- @
-- ∀vs. \"\" `~>=` vs = `Data.Monoid.mempty`
-- ∀pkg. pkg ≠ \"\" ⇒ pkg `~>=` [] = pkg
-- @
(~>=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~>= v
  | Strict.Text.null pkg = Template.empty
  | List.null v          = lit pkg
  | otherwise            = lit pkg <> strLit " >= " <> versionFields v

-- }}} Version Combinators ----------------------------------------------------

-- {{{ Specialized Folds for Template -----------------------------------------

-- | Put specified text between templates.
--
-- >>> separatedBy ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- Be aware that empty values are skipped:
--
-- >>> separatedBy ", " []
-- ""
-- >>> separatedBy ", " ["", ""]
-- ""
-- >>> separatedBy ", " ["foo", mempty, "bar", mempty, "baz", mempty]
-- "foo, bar, baz"
--
-- === Properties:
--
-- Following properties hold:
--
-- @
-- ∀ s. `separatedBy` s [] ≡ `Data.Monoid.mempty`
-- ∀ s t. `separatedBy` s [t] ≡ t
-- ∀ s t₀…tᵢ…tₙ. tᵢ = `Data.Monoid.mempty` ⇒ `separatedBy` s [t₀,…,tᵢ,…,tₙ] = `separatedBy` s [t₀,…,tᵢ₋₁,tᵢ₊₁,…,tₙ]
-- @
separatedBy :: Strict.Text -> [PkgTemplate] -> PkgTemplate
separatedBy s = \case
    -- The complex pattern matching is so that we can avoid doing something
    -- like `List.filter (not . Template.null)` before.
    []                   -> Template.empty
    [x]                  -> x
    Template [] : xs     -> separatedBy s xs
    x : Template [] : xs -> separatedBy s (x : xs)
    x : xs               -> x <> lit s <> separatedBy s xs

-- | Concatenate templates by inserting coma (\',\') in between.
--
-- >>> list ["foo" ~= [1,2,3], "bar" ~> [0], "bar" ~< [3,1]]
-- "foo = 1.2.3, bar > 0, bar < 3.1"
--
-- Be aware that empty values are skipped:
--
-- >>> list []
-- ""
-- >>> list [""]
-- ""
-- >>> list ["", ""]
-- ""
-- >>> list ["", "tinfo" ~> [5,0], "", "libssh" ~> [0, 11]]
-- "tinfo > 5.0, libssh > 0.11"
--
-- === Properties:
--
-- Following properties hold:
--
-- @
-- `list` [] ≡ `Data.Monoid.mempty`
-- ∀ t. `list` [t] ≡ t
-- ∀ t₀…tᵢ…tₙ. tᵢ = `Data.Monoid.mempty` ⇒ `list` [t₀,…,tᵢ,…,tₙ] = `list` [t₀,…,tᵢ₋₁,tᵢ₊₁,…,tₙ]
-- @
list :: [PkgTemplate] -> PkgTemplate
list = separatedBy ", "

-- | Concatenate templates by inserting space (\' \') in between.
--
-- >>> options ["-I" <> var "prefix" </> "lib", "-I" <> var "extra"]
-- "-I${prefix}/lib -I${extra}"
--
-- Be aware that empty values are skipped:
--
-- >>> options []
-- ""
-- >>> options [""]
-- ""
-- >>> options ["", ""]
-- ""
-- >>> options ["-I" <> var "prefix" </> "include", ""]
-- "-I${prefix}/include"
--
-- === Properties:
--
-- Following properties hold:
--
-- @
-- `options` [] ≡ `Data.Monoid.mempty`
-- ∀ t. `options` [t] ≡ t
-- ∀ t₀…tᵢ…tₙ. tᵢ = `Data.Monoid.mempty` ⇒ `options` [t₀,…,tᵢ,…,tₙ] = `options` [t₀,…,tᵢ₋₁,tᵢ₊₁,…,tₙ]
-- @
options :: [PkgTemplate] -> PkgTemplate
options = separatedBy (Strict.Text.singleton ' ')

-- }}} Specialized Folds for Template -----------------------------------------

-- {{{ Options Combinators ----------------------------------------------------

-- | Create template starting with option followed by its argument. Argument
-- is quoted using `quote` function to prevent problems with spaces in
-- directory names.
--
-- >>> option "--foo=" $ var "prefix" </> "some dir"
-- "--foo=\"${prefix}/some dir\""
-- >>> option "" "some/dir"
-- "some/dir"
-- >>> option "-I" mempty
-- "-I\"\""
--
-- === Properties:
--
-- Following property holds:
--
-- @
-- ∀ t. `option` \"\" t ≡ `quote` t
-- @
option :: Strict.Text -> PkgTemplate -> PkgTemplate
option opt = (lit opt <>) . quote

-- | Same as `option`, but takes `String` instead of strict `Strict.Text`.
strOption :: String -> PkgTemplate -> PkgTemplate
strOption = option . Strict.Text.pack

-- | Take list of templates and make compiler include options. Template for
-- include directory is wrapped in quotes (see `quote` and `option` functions).
--
-- >>> let d = var "prefix" </> "include" in includes [d, d </> var "arch"]
-- "-I\"${prefix}/include\" -I\"${prefix}/include/${arch}\""
-- >>> includes [var "prefix" </> "some dir"]
-- "-I\"${prefix}/some dir\""
--
-- Be aware that empty values are skipped:
--
-- >>> includes []
-- ""
-- >>> includes [""]
-- ""
-- >>> includes ["", ""]
-- ""
-- >>> includes [var "prefix", "", var "prefix" </> "x86_64", ""]
-- "-I\"${prefix}\" -I\"${prefix}/x86_64\""
includes :: [PkgTemplate] -> PkgTemplate
includes = options . List.concatMap \arg ->
    [if Template.null arg then Template.empty else option "-I" arg]

-- | Take list of templates and make compiler library options.
--
-- >>> libraries ["m", "rt", "tinfo"]
-- "-lm -lrt -tinfo"
--
-- Be aware that empty values are skipped:
--
-- >>> libraries []
-- ""
-- >>> libraries [""]
-- ""
-- >>> libraries ["", ""]
-- ""
-- >>> libraries ["m", "", "tinfo", ""]
-- "-lm -ltinfo"
libraries :: [PkgTemplate] -> PkgTemplate
libraries = options . List.concatMap \arg ->
    [if Template.null arg then Template.empty else strLit "-l" <> arg]

-- | Take list of templates and make compiler library path options. Template
-- for include directory is wrapped in quotes (see `quote` and `option`
-- functions).
--
-- >>> let l = var "prefix" </> lit "lib" in libraryPath [l, l </> var "arch"]
-- "-L\"${prefix}/lib\" -L\"${prefix}/lib/${arch}\""
--
-- Be aware that empty values are skipped:
--
-- >>> libraryPath []
-- ""
-- >>> libraryPath [""]
-- ""
-- >>> libraryPath ["", ""]
-- ""
-- >>> libraryPath [var "prefix" </> "lib", "", var "prefix" </> "lib" </> "x86_64"]
-- "-L\"${prefix}/lib\" -L\"${prefix}/lib/x86_64\""
libraryPath :: [PkgTemplate] -> PkgTemplate
libraryPath = options . List.concatMap \arg ->
    [if Template.null arg then Template.empty else option "-L" arg]

-- }}} Options Combinators ----------------------------------------------------
-- }}} PkgTemplate Combinators ------------------------------------------------

-- {{{ Serialization ----------------------------------------------------------

-- | Serialize `PkgConfig` into a `String`.
toString :: PkgConfig -> String
toString = Strict.Text.unpack . toStrictText

-- }}} Serialization ----------------------------------------------------------

-- {{{ I/O --------------------------------------------------------------------

-- | Serialize `PkgConfig` into strict `Strict.Text` and write it into a
-- specified file.
writePkgConfig :: MonadIO io => FilePath -> PkgConfig -> io ()
writePkgConfig file = liftIO . Strict.Text.writeFile file . toStrictText

-- | Serialize `PkgConfig` into strict `Strict.Text` and write it to a
-- specified `Handle`.
hPutPkgConfig :: MonadIO io => Handle -> PkgConfig -> io ()
hPutPkgConfig h = liftIO . Strict.Text.hPutStr h . toStrictText

-- }}} I/O --------------------------------------------------------------------

-- $usage
--
-- Following code is able to generate @foo.pc@, a /pkg-config/ configuration
-- file for library named @foo@:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import "Data.String" (`Data.String.IsString`)
--
-- import System.FilePath (addExtension)
--   -- From filepath library:
--   -- <http://hackage.haskell.org/package/filepath>
--
-- import "Data.PkgConfig"
--
--
-- libBaseName :: `Data.String.IsString` a => a
-- libBaseName = \"foo\"
--
-- main :: IO ()
-- main = `writePkgConfig` (libBaseName \`addExtension\` \"pc\") libPkgConfig
--   where
--     libPkgConfig = `empty`
--         { `variables`   =
--             [ \"prefix\"     `.=` \"\/usr\/local\"
--             , \"includedir\" `.=` (`var` \"prefix\" `</>` \"include\")
--             , \"libdir\"     `.=` (`var` \"prefix\" `</>` \"lib\")
--             , \"arch\"       `.=` \"i386\"
--             ]
--         , `name`        = libBaseName
--         , `description` = \"Example pkg-config.\"
--         , `version`     = `ver` [1, 2, 3 :: Word]
--         , `cflags`      = `includes` [`var` \"includedir\"]
--         , `requires`    = `list` ["tinfo" ~> [1, 0]]
--         , `libs`        = `options`
--             [ `libraryPath`
--                 [ `var` \"libdir\"
--                 , `var` \"libdir\" `</>` `var` \"arch\"
--                 ]
--             , `libraries` [libBaseName]
--             ]
--         }
-- @
--
-- Content of generated @foo.pc@:
--
-- > prefix=/usr/local
-- > includedir=${prefix}/include
-- > libdir=${prefix}/lib
-- > arch=i386
-- >
-- > Name: foo
-- > Description: Example pkg-config.
-- > Version: 1.2.3
-- > Requires: tinfo > 1.0
-- > Cflags: -I"${includedir}"
-- > Libs: -L"${libdir}" -L"${libdir}/${arch}" -lfoo

-- $pkg-config
--
-- Data type that describes whole /pkg-config/ configuration file for one
-- specific library. It also tries to preserve as much of /pkg-config/ \/
-- /pkgconf/ philosophy as possible.
--
-- @
-- `empty`
--     { `variables` = [\"prefix\" `.=` \"\/usr\/local\"]
--     , `name`      = \"some library\"
--     -- ...
--     , `libs`      = `includes`
--         [ `var` \"prefix\" `</>` \"include\" `</>` \"foo\"
--         ]
--     }
-- @
--
-- There is another approach that can be useful in some cases.
--
-- @
-- `empty` & \\`PkgConfig`{..} -> `PkgConfig`
--     { `variables` = [\"prefix\" `.=` \"\/usr\/local\"]
--     , `name`      = \"some library\"
--     -- ...
--     , `libs`      = `includes`
--         [ `var` \"prefix\" `</>` \"include\" `</>` \"foo\"
--         ]
--     , ..
--     }
-- @
--
-- Lenses can be used for accessing individual fields of `PkgConfig` data type.
-- Example:
--
-- @
-- `empty`
--     & `pkgVariables` .~ [\"prefix\" `.=` \"\/usr\/local\"]
--     & `pkgName`      .~ \"some library\"
--     -- ...
--     & `pkgLibs`      .~ `includes`
--         [ `var` \"prefix\" `</>` \"include\" `</>` \"foo\"
--         ]
-- @
--
-- Note that function @.~@, used in the example, is from [@lens@ library
-- ](http://hackage.haskell.org/package/lens). Please consult its documentation
-- for details.

-- $pkg-template
--
-- The /pkg-config/ tool allows variable declaration so that they can later be
-- used in other parts of its configuration file. To give Haskell programmer
-- the same power, this library provides `PkgTemplate`. One can think of it as
-- a string with named holes, i.e. places where variables will be expanded.
-- Following is example of how two variables, namely @prefix@ and @includedir@,
-- can be defined inside /pkg-config/ configuration file:
--
-- > prefix=/usr/local
-- > includedir=${prefix}/include
--
-- `PkgConfig` has a field `variables` that is used to define variables and
-- above example can be translated into:
--
-- @
-- `empty`
--     { `variables` =
--         [ \"prefix\" `.=` \"\/usr\/local\"
--         , \"includedir\" `.=` `var` \"prefix\" `</>` \"include\"
--         ]
--     }
-- @
--
-- Lot of similar properties of `String` hold for `Template` as well.
-- Including the fact that `Template` is monoid and therefore can be
-- concatenated using monoid operations:
--
-- >>> strLit "foo" <> strLit "bar"
-- "foobar"
--
-- Since `PkgTemplate` has `Data.String.IsString` instance, then, if
-- @OverloadedStrings@ language extension is enabled, it is possible to
-- simplify above example into:
--
-- >>> "foo" <> "bar" :: PkgTemplate
-- "foobar"
--
-- On the other hand variables behave slightly differently, yet the
-- `PkgTemplate` still forms a `Data.Monoid.Monoid`:
--
-- >>> var "prefix" <> strLit "foo"
-- "${prefix}foo"
--
-- Additionally following properties hold:
--
-- @
-- `lit` \"\" ≡ `Data.Monoid.mempty`
-- `var` \"\" ≡ `Data.Monoid.mempty`
-- @
