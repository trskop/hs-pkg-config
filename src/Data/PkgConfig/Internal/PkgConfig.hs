{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type describing pkg-config configuration file
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Data type describing /pkg-config/ configuration file.
module Data.PkgConfig.Internal.PkgConfig
    (
      PkgConfig(..)

    , PkgVariable
    , PkgName
    , PkgDescription
    , PkgUrl
    , PkgVersion

    , toStrictText
    )
  where

import Data.Bool (not, otherwise)
import Data.Data (Data)
import Data.Function ((.), ($))
import Data.List as List (filter, map)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Typeable (Typeable)
import Text.Show (Show)

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text

import Data.Default.Class (Default(def))

import Data.PkgConfig.Internal.Template (PkgTemplate)
import qualified Data.PkgConfig.Internal.Template as Template (toStrictText)


type PkgVariable = (Strict.Text, PkgTemplate)
type PkgName = Strict.Text
type PkgDescription = Strict.Text
type PkgUrl = Strict.Text
type PkgVersion = PkgTemplate

data PkgConfig = PkgConfig
    { _pkgVariables :: [PkgVariable]
    -- ^ Variable definitions.

    , _pkgName :: PkgName
    -- ^ Human-readable name for the library or package. This field is not used
    -- by pkg-config tool for queries, because it uses @.pc@ file base name.

    , _pkgDescription :: PkgDescription
    -- ^ Brief description of the package.

    , _pkgUrl :: PkgUrl
    -- ^ URL where people can get more information about and download the
    -- package.

    , _pkgVersion :: PkgVersion
    -- ^ Version of the package.

    , _pkgRequires :: PkgTemplate
    -- ^ List of packages required by this package and their version bounds.

    , _pkgRequiresPrivate :: PkgTemplate
    -- ^ List of private packages required by this package but not exposed to
    -- applications. The version specific rules from the Requires field also
    -- apply here.

    , _pkgConflicts :: PkgTemplate
    -- ^ An optional field describing packages that this one conflicts with.
    -- The version specific rules from the Requires field also apply here. This
    -- field also takes multiple instances of the same package. E.g.,
    -- Conflicts: bar < 1.2.3, bar >= 1.3.0.

    , _pkgCflags :: PkgTemplate
    -- ^ Compiler flags specific to this package and any required libraries
    -- that don't support pkg-config. If the required libraries support
    -- pkg-config, they should be added to Requires or Requires.private.

    , _pkgLibs :: PkgTemplate
    -- ^ Linking flags specific to this package and any required libraries that
    -- don't support pkg-config. The same rules as for @Cflags@ ('_pkgCflags')
    -- field apply here.

    , _pkgLibsPrivate :: PkgTemplate
    -- ^ Linking flags for private libraries required by this package but not
    -- exposed to applications. The same rules as for @Cflags@ field apply
    -- here.
    }
  deriving (Data, Show, Typeable)

instance Default PkgConfig where
    def = PkgConfig
        { _pkgVariables = []
        , _pkgName = Strict.Text.empty
        , _pkgDescription = Strict.Text.empty
        , _pkgUrl = Strict.Text.empty
        , _pkgVersion = mempty
        , _pkgRequires = mempty
        , _pkgRequiresPrivate = mempty
        , _pkgConflicts = mempty
        , _pkgCflags = mempty
        , _pkgLibs = mempty
        , _pkgLibsPrivate = mempty
        }

-- | Serialize 'PkgConfig' in to strict 'Strict.Text'.
toStrictText :: PkgConfig -> Strict.Text
toStrictText PkgConfig{..} = Strict.Text.concat
    [ variablesToStrictText _pkgVariables
    , Strict.Text.unlines [""]
    , Strict.Text.unlines $ List.filter (not . Strict.Text.null)
        [ "Name"             <:>  _pkgName
        , "Description"      <:>  _pkgDescription
        , "URL"              <:>  _pkgUrl
        , "Version"          <:>. _pkgVersion
        , "Requires"         <:>. _pkgRequires
        , "Requires.private" <:>. _pkgRequiresPrivate
        , "Conflicts"        <:>. _pkgConflicts
        , "Cflags"           <:>. _pkgCflags
        , "Libs"             <:>. _pkgLibs
        , "Libs.private"     <:>. _pkgLibsPrivate
        ]
    ]
  where
    key <:> value
      | Strict.Text.null value = Strict.Text.empty
      | otherwise              = key <> ": " <> value

    key <:>. value = key <:> Template.toStrictText value

    variablesToStrictText = Strict.Text.unlines . map varToText
      where
        varToText (name, tmpl) = name <> "=" <> Template.toStrictText tmpl
