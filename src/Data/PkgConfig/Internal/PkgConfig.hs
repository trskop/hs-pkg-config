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
    -- * PkgConfig
      PkgConfig(..)

    -- * Type Aliases
    , PkgVariable
    , PkgName
    , PkgDescription
    , PkgUrl
    , PkgVersion

    -- * Lenses
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

    -- * Serialization
    , toStrictText
    )
  where

import Data.Bool (not, otherwise)
import Data.Data (Data)
import Data.Function ((.), ($), flip)
import Data.Functor (Functor(fmap))
import Data.List as List (filter, map)
import Data.Monoid ((<>))
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
    -- @Conflicts: bar < 1.2.3, bar >= 1.3.0.@

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
        , _pkgVersion = def
        , _pkgRequires = def
        , _pkgRequiresPrivate = def
        , _pkgConflicts = def
        , _pkgCflags = def
        , _pkgLibs = def
        , _pkgLibsPrivate = def
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

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

pkgVariables
    :: Functor f
    => ([PkgVariable] -> f [PkgVariable])
    -> PkgConfig -> f PkgConfig
pkgVariables f cfg@(PkgConfig{_pkgVariables = a}) =
    f a <$$> \b -> cfg{_pkgVariables = b}

pkgName
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgName f cfg@(PkgConfig{_pkgName = a}) =
    f a <$$> \b -> cfg{_pkgName = b}

pkgDescription
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgDescription f cfg@(PkgConfig{_pkgDescription = a}) =
    f a <$$> \b -> cfg{_pkgDescription = b}

pkgUrl
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgUrl f cfg@(PkgConfig{_pkgUrl = a}) =
    f a <$$> \b -> cfg{_pkgUrl = b}

pkgVersion
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgVersion f cfg@(PkgConfig{_pkgVersion = a}) =
    f a <$$> \b -> cfg{_pkgVersion = b}

pkgRequires
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequires f cfg@(PkgConfig{_pkgRequires = a}) =
    f a <$$> \b -> cfg{_pkgRequires = b}

pkgRequiresPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresPrivate f cfg@(PkgConfig{_pkgRequiresPrivate = a}) =
    f a <$$> \b -> cfg{_pkgRequiresPrivate = b}

pkgConflicts
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgConflicts f cfg@(PkgConfig{_pkgConflicts = a}) =
    f a <$$> \b -> cfg{_pkgConflicts = b}

pkgCflags
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgCflags f cfg@(PkgConfig{_pkgCflags = a}) =
    f a <$$> \b -> cfg{_pkgCflags = b}

pkgLibs
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibs f cfg@(PkgConfig{_pkgLibs = a}) =
    f a <$$> \b -> cfg{_pkgLibs = b}

pkgLibsPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibsPrivate f cfg@(PkgConfig{_pkgLibsPrivate = a}) =
    f a <$$> \b -> cfg{_pkgLibsPrivate = b}
