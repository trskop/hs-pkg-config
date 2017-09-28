{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Create pkg-config configuration files
-- Copyright:    (c) 2014, 2017 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Create /pkg-config/ configuration file from Haskell code using combinators
-- specialized for this purpose. To learn more about /pkg-config/ please read
-- one or both following articles:
--
-- * <http://people.freedesktop.org/~dbn/pkg-config-guide.html Guide to pkg-config>
--   by Dan Nicholson
--
-- * <https://autotools.io/ Autotools Mythbuster> by Diego Elio Pettenò:
--   <https://autotools.io/pkgconfig/index.html Chapter 4. Dependency discovery -- pkg-config>
module Data.PkgConfig
    (
    -- * Usage
    -- $usage

    -- * PkgConfig
    -- $pkgConfig
      PkgConfig

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

    -- ** Type Aliases
    --
    -- | These are used to make type signatures easier to read.
    , PkgDescription
    , PkgName
    , PkgUrl
    , PkgVariable
    , PkgVersion

    -- ** Serialization
    , toStrictText
    , toString

    -- ** I/O
    , writePkgConfig

    -- * PkgTemplate
    -- $pkgTemplate
    , PkgTemplate

    -- ** Smart Constructors
    , var
    , lit
    , strLit
    , singletonLit

    -- ** Combinators
    , quote

    -- ** FilePath-like Combinators
    , (</>)
    , (<.>)

    -- ** Version Combinators
    , version
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
    , variables
    )
  where

import Data.Function ((.), ($))
import Data.Int (Int)
import Data.List as List (map)
import Data.Monoid (Monoid(mempty), (<>))
import Data.String (String)
import Data.Word (Word)
import System.IO (IO, FilePath)
import Text.Show (Show(show))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text (pack, singleton, unpack)
import qualified Data.Text.IO as Strict.Text (writeFile)

import Data.PkgConfig.Internal.Template
    ( PkgTemplate
    , lit
    , singletonLit
    , strLit
    , var
    , variables
    )
import Data.PkgConfig.Internal.PkgConfig
    ( PkgConfig
    , PkgDescription
    , PkgName
    , PkgUrl
    , PkgVariable
    , PkgVersion
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

-- {{{ PkgTemplate Combinators ------------------------------------------------

-- | Put quotation marks (\'\"\') around a template.
--
-- >>> quote $ var "prefix" </> "include"
-- "${prefix}/include"
--
-- >>> var "prefix" </> quote "dir with spaces"
-- ${prefix}/"dir with spaces"
quote :: PkgTemplate -> PkgTemplate
quote t = singletonLit '"' <> t <> singletonLit '"'

-- {{{ File Path Combinators --------------------------------------------------

-- | Put literal \"/\" between two templates.
--
-- >>> var "prefix" </> lit "foo" <.> lit "pc"
-- ${prefix}/foo.pc
(</>) :: PkgTemplate -> PkgTemplate -> PkgTemplate
t1 </> t2 = t1 <> singletonLit '/' <> t2

-- | Put literal \".\" between two templates.
--
-- >>> var "prefix" </> lit "foo" <.> lit "pc"
-- ${prefix}/foo.pc
-- >>> var "major" <.> var "minor" <.> var "patch"
-- ${major}.${minor}.${patch}
(<.>) :: PkgTemplate -> PkgTemplate -> PkgTemplate
t1 <.> t2 = t1 <> singletonLit '.' <> t2

-- }}} File Path Combinators --------------------------------------------------

-- {{{ Version Combinators ----------------------------------------------------

-- | Treat list of integers as version number and construct template literal
-- out of it.
--
-- >>> version [1, 2, 3]
-- 1.2.3
-- >>> version [] == mempty
-- True
version :: [Word] -> PkgTemplate
version = \case
    []     -> mempty
    v : vs -> case vs of
        [] -> wordLit v
        _  -> wordLit v <.> version vs
  where
    wordLit :: Word -> PkgTemplate
    wordLit = strLit . show

-- | Variant of 'version' that takes list of integers. This function can be
-- used to create 'PkgTemplate' from standard Haskell 'Data.Version.Version'
-- data type.
--
-- >>> versionInt . versionBranch $ Version [0, 1, 2] []
-- 0.1.2
versionInt :: [Int] -> PkgTemplate
versionInt = \case
    []     -> mempty
    v : vs -> case vs of
        [] -> intLit v
        _  -> intLit v <.> versionInt vs
  where
    intLit :: Int -> PkgTemplate
    intLit = strLit . show

-- | Dependency on a package of exact version.
--
-- >>> "sqlite" ~= [3, 8, 7, 1]
-- sqlite = 3.8.7.1
--
-- >>> list ["sqlite" ~= [3, 8, 7, 1], "alpha" ~= [7, 2]]
-- sqlite = 3.8.7.1, alpha = 7.2
(~=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~=  ver = lit pkg <> strLit " = "  <> version ver

-- | Dependency on a package not of a specific version.
--
-- >>> "alpha" ~/= [7, 2]
-- alpha != 7.2
--
-- >>> list ["sqlite" ~/= [3, 8, 7, 1], "alpha" ~/= [7, 2]]
-- sqlite != 3.8.7.1, alpha != 7.2
(~/=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~/= ver = lit pkg <> strLit " != " <> version ver

-- | Dependency on a package with version greater or less then specified
-- value.
--
-- >>> "alpha" ~< [7, 3]
-- alpha < 7.3
--
-- >>> list ["sqlite" ~< [3, 9], "alpha" ~< [7, 3]]
-- sqlite < 3.9, alpha < 7.3
(~<) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~<  ver = lit pkg <> strLit " < "  <> version ver

-- | Dependency on a package with version greater then specified value.
--
-- >>> "sqlite" ~> [3, 8]
-- sqlite3 > 3.8
--
-- >>> list ["sqlite" ~> [3, 8], "alpha" ~> [7, 1]]
-- sqlite > 3.8, alpha > 7.1
(~>) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~>  ver = lit pkg <> strLit " > "  <> version ver

-- | Dependency on a package with version greater or less or equal then
-- specified value.
(~<=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~<= ver = lit pkg <> strLit " <= " <> version ver

-- | Dependency on a package with version greater or equal then specified
-- value.
(~>=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~>= ver = lit pkg <> strLit " >= " <> version ver

-- }}} Version Combinators ----------------------------------------------------

-- {{{ Specialized Folds for Template -----------------------------------------

-- | Put specified text between templates.
--
-- Following properties hold:
--
-- @
-- forall s. 'separatedBy' s [] === 'mempty'
-- forall s t. 'separatedBy' s [t] === t
-- @
--
-- Example:
--
-- >>> separatedBy ", " ["foo", "bar", "baz"]
-- foo, bar, baz
separatedBy :: Strict.Text -> [PkgTemplate] -> PkgTemplate
separatedBy s = \case
    []     -> mempty
    [x]    -> x
    x : xs -> x <> lit s <> separatedBy s xs

-- | Concatenate templates by inserting coma (\',\') in between.
--
-- >>> list ["foo" .= [1,2,3], "bar" .> [0], "bar" .< [3,1]]
-- foo = 1.2.3, bar > 0, bar < 3.1
--
-- Following properties hold:
--
-- @
-- 'list' [] === 'mempty'
-- forall t. 'list' [t] === t
-- @
list :: [PkgTemplate] -> PkgTemplate
list = separatedBy $ Strict.Text.pack ", "

-- | Concatenate templates by inserting space (\' \') in between.
--
-- >>> options ["-I" <> var "prefix" </> "lib", "-I" <> var "extra"]
-- -I${prefix}/lib -I${extra}
--
-- Following properties hold:
--
-- @
-- 'options' [] === 'mempty'
-- forall t. 'options' [t] === t
-- @
options :: [PkgTemplate] -> PkgTemplate
options = separatedBy $ Strict.Text.singleton ' '

-- }}} Specialized Folds for Template -----------------------------------------

-- {{{ Options Combinators ----------------------------------------------------

-- | Create template starting with option followed by its argument. Argument
-- is quoted using 'quote' function to prevent problems with spaces in
-- directory names.
--
-- >>> option "--foo=" $ var "prefix" </> "some dir"
-- --foo="${prefix}/some dir"
--
-- Following property holds:
--
-- @
-- forall t. 'option' \"\" t === 'quote' t
-- @
option :: Strict.Text -> PkgTemplate -> PkgTemplate
option opt = (lit opt <>) . quote

-- | Same as 'option', but takes 'String' instead of strict 'Strict.Text'.
strOption :: String -> PkgTemplate -> PkgTemplate
strOption = option . Strict.Text.pack

-- | Take list of templates and make compiler include options. Template for
-- include directory is wrapped in quotes (see 'quote' and 'option' functions).
--
-- >>> let d = var "prefix" </> "include" in includes [d, d </> var "arch"]
-- -I"${prefix}/include" -I"${prefix}/include/${arch}"
-- >>> includes [var "prefix" </> "some dir"]
-- -I"${prefix}/some dir"
includes :: [PkgTemplate] -> PkgTemplate
includes = options . List.map (strOption "-I")

-- | Take list of templates and make compiler library options.
--
-- >>> libraries ["m", "rt", "foo"]
-- -lm -lrt -lfoo
libraries :: [PkgTemplate] -> PkgTemplate
libraries = options . List.map (strLit "-l" <>)

-- | Take list of templates and make compiler library path options. Template for
-- include directory is wrapped in quotes (see 'quote' and 'option' functions).
--
-- >>> let l = var "prefix" </> lit "lib" in libraryPath [l, l </> var "arch"]
-- -L"${prefix}/lib" -L"${prefix}/lib/${arch}"
libraryPath :: [PkgTemplate] -> PkgTemplate
libraryPath = options . List.map (strOption "-L")

-- }}} Options Combinators ----------------------------------------------------
-- }}} PkgTemplate Combinators ------------------------------------------------

-- {{{ Serialization ----------------------------------------------------------

-- | Serialize 'PkgConfig' in to strict 'Strict.Text' and then convert it to a
-- 'String'.
toString :: PkgConfig -> String
toString = Strict.Text.unpack . toStrictText

-- }}} Serialization ----------------------------------------------------------

-- {{{ I/O --------------------------------------------------------------------

-- | Serialize 'PkgConfig' in to strict 'Strict.Text' and write it in to a
-- specified file.
writePkgConfig :: FilePath -> PkgConfig -> IO ()
writePkgConfig file = Strict.Text.writeFile file . toStrictText

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
-- import Data.String ('IsString')
--
-- import Data.Default.Class ('Default'('def'))
--   -- From data-default-class library:
--   -- <http://hackage.haskell.org/package/data-default-class>
--
-- import Control.Lens
--   -- From lens library:
--   -- <http://hackage.haskell.org/package/lens>
--
-- import Data.PkgConfig
--
--
-- libraryBaseName :: 'IsString' a => a
-- libraryBaseName = \"foo\"
--
-- main :: IO ()
-- main = 'writePkgConfig' (libraryBaseName '++' \".pc\") libPkgConfig
--   where
--     libPkgConfig = 'def'
--         & 'pkgVariables'   .~
--             [ (\"prefix\",     \"\/usr\/local\"              )
--             , (\"includedir\", 'var' \"prefix\" '</>' \"include\")
--             , (\"libdir\",     'var' \"prefix\" '</>' \"lib\"    )
--             , (\"arch\",       \"i386\"                    )
--             ]
--         & 'pkgName'        .~ libraryBaseName
--         & 'pkgDescription' .~ \"Example pkg-config.\"
--         & 'pkgVersion'     .~ 'version' [1, 2, 3]
--         & 'pkgCflags'      .~ 'includes' ['var' \"includedir\"]
--         & 'pkgRequires'    .~ 'list'
--             [ \"bar\" '~>' [0], \"bar\" '~<=' [3, 1]
--             , \"baz\" '~=' [1, 2, 3]
--             ]
--         & 'pkgLibs'        .~ 'options'
--             [ 'libraryPath' ['var' \"libdir\", 'var' \"libdir\" '</>' 'var' \"arch\"]
--             , 'libraries' [libraryBaseName]
--             ]
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
-- > Requires: bar > 0, bar <= 3.1, baz = 1.2.3
-- > Cflags: -I"${includedir}"
-- > Libs: -L"${libdir}" -L"${libdir}/${arch}" -lfoo
--
-- Note that functions '&' and '.~', used in the example, are from
-- <http://hackage.haskell.org/package/lens lens> library. Please consult its
-- documentation for details.

-- $pkgConfig
--
-- Data type that describes whole /pkg-config/ configuration file for one
-- specific library. It also tries to preserve as much of /pkg-config/
-- philosophy as possible.
--
-- Lenses are used for accessing individual fields of 'PkgConfig' data type.
-- Example:
--
-- @
-- 'def' & 'pkgVariables' .~ [(\"prefix\", \"\/usr\/local\")]
--     & 'pkgName'      .~ \"some library\"
--     -- ...
--     & 'pkgLibs'      .~ 'includes'
--         [ 'var' \"prefix\" '</>' \"include\" '</>' \"foo\"
--         ]
-- @

-- $pkgTemplate
--
-- The /pkg-config/ tool allows variable declaration so that they can later be
-- used in other parts of its configuration file. To give Haskell programmer
-- the same power, this library provides 'PkgTemplate'. One can think of it as
-- a string with named holes, i.e. places where variables will be expanded.
-- Following is example of how two variables, namely @prefix@ and @includedir@,
-- can be defined inside /pkg-config/ configuration file:
--
-- > prefix=/usr/local
-- > includedir=${prefix}/include
--
-- 'PkgConfig' has a field 'pkgVariables' that is used to define variables and
-- above example can be translated in to:
--
-- @
-- 'def' & 'pkgVariables' .~
--     [ (\"prefix\", \"\/usr\/local\")
--     , (\"includedir\", 'var' \"prefix\" '</>' \"include\")
--     ]
-- @
--
-- Lot of similar properties of 'String' hold for 'Template' as well.
-- Including the fact that 'Template' is monoid and therefore can be
-- concatenated using monoid operations:
--
-- >>> strLit "foo" <> strLit "bar"
-- foobar
--
-- Since 'Template' has 'IsString' instance, then, if @OverloadedStrings@
-- language extension is enabled, it is possible to simplify above example in
-- to:
--
-- >>> "foo" <> "bar" :: PkgTemplate
-- foobar
--
-- For consistency instance for 'Default' type class is also provided and it
-- holds following property:
--
-- @
-- 'def' === 'mempty'
-- @
--
-- Additionally following properties hold:
--
-- @
-- 'lit' \"\" === 'mempty'
-- 'var' \"\" =/= 'mempty'
-- @
