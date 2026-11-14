-- |
-- Module:       $HEADER$
-- Description:  Data type describing pkg-config configuration file
-- Copyright:    (c) 2014, 2017, 2026 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- __TL;DR:__ Data type describing /pkg-config/ configuration file.
module Data.PkgConfig.Internal.PkgConfig
    (
    -- * PkgConfig
      PkgConfig(..)
    , empty

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
    , pkgProvides
    , pkgCflags
    , pkgCflagsPrivate
    , pkgLibs
    , pkgLibsPrivate

    -- * Serialization
    , toStrictText
    )
  where

import Data.Bool (not, otherwise)
import Data.Data (Data)
import Data.Function ((.), ($))
import Data.Functor (Functor, (<&>))
import Data.List as List (filter, map, null)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Text qualified as Strict (Text)
import Data.Text qualified as Strict.Text

import Data.PkgConfig.Internal.Template (PkgTemplate)
import Data.PkgConfig.Internal.Template qualified as Template
    ( empty
    , toStrictText
    )


-- | Variable definition consisting of its name and value in form of
-- 'PkgTemplate'.
type PkgVariable = (Strict.Text, PkgTemplate)

-- | Package name.
type PkgName = Strict.Text

-- | Brief package description.
type PkgDescription = Strict.Text

-- | URL where more package information can be obtained.
type PkgUrl = Strict.Text

-- | Package version may use variable expansion and so it is represented by
-- 'PkgConfig'.
type PkgVersion = PkgTemplate

-- | Representation of /pkg-config/ configuration file.
data PkgConfig = PkgConfig
    { variables :: [PkgVariable]
    -- ^ Variable definitions.

    , name :: PkgName
    -- ^ Human-readable name for a library or package. This field is not used
    -- by /pkg-config/ tool for queries, because it uses @.pc@ file base name.

    , description :: PkgDescription
    -- ^ Brief description of the package.

    , url :: PkgUrl
    -- ^ URL where more package information can be obtained and where the
    -- package can be downloaded.

    , version :: PkgVersion
    -- ^ Version of the package.

    , requires :: PkgTemplate
    -- ^ List of packages required by this package and their version bounds.

    , requiresPrivate :: PkgTemplate
    -- ^ List of private packages required by this package but not exposed to
    -- applications. The version specific rules from the Requires field also
    -- apply here.

    , conflicts :: PkgTemplate
    -- ^ An optional field describing packages that this one conflicts with.
    -- The version specific rules from the Requires field also apply here. This
    -- field also takes multiple instances of the same package. E.g.:
    --
    -- @
    -- Conflicts: bar \< 1.2.3, bar \>= 1.3.0.
    -- @

    , provides :: PkgTemplate
    -- ^ Dependencies that the package fills and that may be provided by an
    -- alternate package.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , cflags :: PkgTemplate
    -- ^ Compiler flags specific to this package and any required libraries
    -- that don't support /pkg-config/. If the required libraries support
    -- /pkg-config/, they should be added to @Requires@ (@requires@ field) or
    -- @Requires.private@ (@requiresPrivate@ field).

    , cflagsPrivate :: PkgTemplate
    -- ^ Compiler flags specific to this package and any required libraries
    -- that don't support /pkg-config/ for when linking statically. If the
    -- required libraries support /pkg-config/, they should be added to
    -- @Requires@ (@requires@ field) or @Requires.private@ (@requiresPrivate@
    -- field).
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , libs :: PkgTemplate
    -- ^ Linking flags specific to this package and any required libraries that
    -- don't support /pkg-config/. The same rules as for @Cflags@ (@cflags@
    -- field) apply here.

    , libsPrivate :: PkgTemplate
    -- ^ Linking flags for private libraries required by this package but not
    -- exposed to applications. The same rules as for @Cflags@ (@cflags@ field)
    -- apply here.
    }
  deriving stock (Data, Generic, Show)

-- | Empty `PkgConfig` with all fields unset or set to an empty value.
empty :: PkgConfig
empty = PkgConfig
    { variables = []
    , name = Strict.Text.empty
    , description = Strict.Text.empty
    , url = Strict.Text.empty
    , version = Template.empty
    , requires = Template.empty
    , requiresPrivate = Template.empty
    , conflicts = Template.empty
    , provides = Template.empty
    , cflags = Template.empty
    , cflagsPrivate = Template.empty
    , libs = Template.empty
    , libsPrivate = Template.empty
    }

-- | Serialize 'PkgConfig' in to strict 'Strict.Text'.
toStrictText :: PkgConfig -> Strict.Text
toStrictText PkgConfig{..} = Strict.Text.concat
    [ variablesToStrictText variables
    , Strict.Text.unlines ["" | not (List.null variables)]
    , Strict.Text.unlines $ List.filter (not . Strict.Text.null)
        [ "Name"             <:>  name
        , "Description"      <:>  description
        , "URL"              <:>  url
        , "Version"          <:>. version
        , "Requires"         <:>. requires
        , "Requires.private" <:>. requiresPrivate
        , "Conflicts"        <:>. conflicts
        , "Provides"         <:>. provides
        , "Cflags"           <:>. cflags
        , "Cflags.private"   <:>. cflagsPrivate
        , "Libs"             <:>. libs
        , "Libs.private"     <:>. libsPrivate
        ]
    ]
  where
    (<:>) :: Strict.Text -> Strict.Text -> Strict.Text
    key <:> value
      | Strict.Text.null value = Strict.Text.empty
      | otherwise              = key <> ": " <> value

    (<:>.) :: Strict.Text -> PkgTemplate -> Strict.Text
    key <:>. value = key <:> Template.toStrictText value

    variablesToStrictText :: [PkgVariable] -> Strict.Text
    variablesToStrictText = Strict.Text.unlines . map varToText
      where
        varToText (var, tmpl) = var <> "=" <> Template.toStrictText tmpl

-- | Variable definitions.
pkgVariables
    :: Functor f
    => ([PkgVariable] -> f [PkgVariable])
    -> PkgConfig -> f PkgConfig
pkgVariables f cfg@PkgConfig{variables = a} = f a <&> \b -> cfg{variables = b}

-- | Human-readable name of a library or package. This field is not used by
-- /pkg-config/ tool for queries, because it uses @.pc@ file base name.
pkgName
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgName f cfg@PkgConfig{name = a} = f a <&> \b -> cfg{name = b}

-- | Brief description of the package.
pkgDescription
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgDescription f cfg@PkgConfig{description = a} =
    f a <&> \b -> cfg{description = b}

-- | URL where people can get more information about and download the package.
pkgUrl
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgUrl f cfg@PkgConfig{url = a} = f a <&> \b -> cfg{url = b}

-- | Version of the package.
pkgVersion
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgVersion f cfg@PkgConfig{version = a} = f a <&> \b -> cfg{version = b}

-- | List of packages required by this package and their version bounds.
pkgRequires
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequires f cfg@PkgConfig{requires = a} = f a <&> \b -> cfg{requires = b}

-- | Compiler flags specific to this package and any required libraries that
-- don't support /pkg-config/. If the required libraries support /pkg-config/,
-- they should be added to @Requires@ ('pkgRequires') or @Requires.private@
-- ('pkgRequiresPrivate').
pkgRequiresPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresPrivate f cfg@PkgConfig{requiresPrivate = a} =
    f a <&> \b -> cfg{requiresPrivate = b}

-- | An optional field describing packages that this one conflicts with. The
-- version specific rules from the Requires field also apply here. This field
-- also takes multiple instances of the same package. E.g.:
--
-- @
-- Conflicts: bar \< 1.2.3, bar \>= 1.3.0.
-- @
pkgConflicts
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgConflicts f cfg@PkgConfig{conflicts = a} =
    f a <&> \b -> cfg{conflicts = b}

-- | Dependencies that the package fills and that may be provided by an
-- alternate package.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgProvides
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgProvides f cfg@PkgConfig{provides = a} = f a <&> \b -> cfg{provides = b}

-- | Compiler flags specific to this package and any required libraries that
-- don't support /pkg-config/. If the required libraries support /pkg-config/,
-- they should be added to @Requires@ ('pkgRequires') or @Requires.private@
-- ('pkgRequiresPrivate').
pkgCflags
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgCflags f cfg@PkgConfig{cflags = a} = f a <&> \b -> cfg{cflags = b}

-- | Compiler flags specific to this package and any required libraries that
-- don't support /pkg-config/ for when linking statically. If the required
-- libraries support /pkg-config/, they should be added to @Requires@
-- ('pkgRequires') or @Requires.private@ ('pkgRequiresPrivate').
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgCflagsPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgCflagsPrivate f cfg@PkgConfig{cflagsPrivate = a} =
    f a <&> \b -> cfg{cflagsPrivate = b}

-- | Linking flags specific to this package and any required libraries that
-- don't support /pkg-config/. The same rules as for @Cflags@ ('pkgCflags')
-- field apply here.
pkgLibs
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibs f cfg@PkgConfig{libs = a} = f a <&> \b -> cfg{libs = b}

-- | Linking flags for private libraries required by this package but not
-- exposed to applications. The same rules as for @Cflags@ ('pkgCflags') field
-- apply here.
pkgLibsPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibsPrivate f cfg@PkgConfig{libsPrivate = a} =
    f a <&> \b -> cfg{libsPrivate = b}
