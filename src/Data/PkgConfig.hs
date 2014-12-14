{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Create pkg-config configuration files
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, NoImplicitPrelude
--
-- Create /pkg-config/ configuration file from Haskell code using combinators
-- specialized for this purpose.
module Data.PkgConfig
    (
    -- * PkgConfig
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
    , PkgDescription
    , PkgName
    , PkgUrl
    , PkgVariable
    , PkgVersion
    , toStrictText

    -- ** I/O
    , writePkgConfig

    -- * PkgTemplate
    , PkgTemplate

    -- ** Smart Constructors
    , var
    , lit
    , strLit
    , singletonLit

    -- ** File Path Combinators
    , (</>)
    , (<.>)

    -- ** Version Combinators
    , version
    , (~=), (~/=), (~<), (~>), (~<=), (~>=)

    -- ** Options Combinators
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
import Data.List as List (map)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Word (Word)
import System.IO (IO, FilePath)
import Text.Show (Show(show))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text (pack, singleton)
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
version []       = mempty
version (v : vs) = case vs of
    [] -> wordLit v
    _  -> wordLit v <.> version vs
  where
    wordLit :: Word -> PkgTemplate
    wordLit = strLit . show

(~=), (~/=), (~<), (~>), (~<=), (~>=) :: Strict.Text -> [Word] -> PkgTemplate
pkg ~=  ver = lit pkg <> strLit " = "  <> version ver
pkg ~/= ver = lit pkg <> strLit " != " <> version ver
pkg ~<  ver = lit pkg <> strLit " < "  <> version ver
pkg ~>  ver = lit pkg <> strLit " > "  <> version ver
pkg ~<= ver = lit pkg <> strLit " <= " <> version ver
pkg ~>= ver = lit pkg <> strLit " >= " <> version ver

-- }}} Version Combinators ----------------------------------------------------

-- {{{ Specialized Folds for Template -----------------------------------------

separatedBy :: Strict.Text -> [PkgTemplate] -> PkgTemplate
separatedBy _ []       = mempty
separatedBy _ (x : []) = x
separatedBy s (x : xs) = x <> lit s <> separatedBy s xs

-- | Concatenate templates by inserting coma (\',\') in between.
--
-- >>> list ["foo" .= [1,2,3], "bar" .> [0], "bar" .< [3,1]]
-- foo = 1.2.3, bar > 0, bar < 3.1
list :: [PkgTemplate] -> PkgTemplate
list = separatedBy $ Strict.Text.pack ", "

-- | Concatenate templates by inserting space (\' \') in between.
--
-- >>> options ["-I" <> var "prefix" </> "lib", "-I" <> var "extra"]
-- -I${prefix}/lib -I${extra}
options :: [PkgTemplate] -> PkgTemplate
options = separatedBy $ Strict.Text.singleton ' '

-- }}} Specialized Folds for Template -----------------------------------------

-- {{{ Options Combinators ----------------------------------------------------

-- | Take list of templates and make compiler include options.
--
-- >>> let d = var "prefix" </> "include" in includes [d, d </> var "arch"]
-- -I${prefix}/include -I${prefix}/include/${arch}
includes :: [PkgTemplate] -> PkgTemplate
includes = options . List.map (strLit "-I" <>)

-- | Take list of templates and make compiler library options.
--
-- >>> libraries ["m", "rt", "foo"]
-- -lm -lrt -lfoo
libraries :: [PkgTemplate] -> PkgTemplate
libraries = options . List.map (strLit "-l" <>)

-- | Take list of templates and make compiler library path options.
--
-- >>> let l = var "prefix" </> lit "lib" in libraryPath [l, l </> var "arch"]
-- -L${prefix}/lib -L${prefix}/lib/${arch}
libraryPath :: [PkgTemplate] -> PkgTemplate
libraryPath = options . List.map (strLit "-L" <>)

-- }}} Options Combinators ----------------------------------------------------

-- {{{ I/O --------------------------------------------------------------------

writePkgConfig :: FilePath -> PkgConfig -> IO ()
writePkgConfig file = Strict.Text.writeFile file . toStrictText

-- }}} I/O --------------------------------------------------------------------
