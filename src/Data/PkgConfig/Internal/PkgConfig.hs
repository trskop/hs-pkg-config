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

import Control.Applicative (pure)
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
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances.Text () -- Instances only

import Data.PkgConfig.Internal.Template (PkgTemplate)
import Data.PkgConfig.Internal.Template qualified as Template
    ( empty
    , toStrictText
    )


-- | Variable definition consisting of its name and value in form of
-- `PkgTemplate`.
type PkgVariable = (Strict.Text, PkgTemplate)

-- | Package name.
type PkgName = Strict.Text

-- | Brief package description.
type PkgDescription = Strict.Text

-- | URL where more package information can be obtained.
type PkgUrl = Strict.Text

-- | Package version may use variable expansion and so it is represented by
-- `PkgConfig`.
type PkgVersion = PkgTemplate

-- | Representation of /pkg-config/ configuration file.
data PkgConfig = PkgConfig
    { variables :: [PkgVariable]
    -- ^ Variable definitions.
    --
    -- @
    -- prefix=\/usr
    -- libdir=${prefix}\/lib\/x86_64
    -- includedir=${prefix}\/include
    -- @

    , name :: PkgName
    -- ^ Human-readable name for a library or package. This field is not used
    -- by /pkg-config/ tool for queries, because it uses @.pc@ file base name.
    --
    -- @
    -- Name: libsomething
    -- @

    , description :: PkgDescription
    -- ^ Brief description of the package.
    --
    -- @
    -- Description: Some example library
    -- @

    , url :: PkgUrl
    -- ^ URL where more package information can be obtained and where the
    -- package can be downloaded.
    --
    -- @
    -- URL: https:\/\/git.example.tld\/org\/project
    -- @

    , version :: PkgVersion
    -- ^ Version of the package.
    --
    -- @
    -- Version: 1.0.11
    -- @

    , requires :: PkgTemplate
    -- ^ List of packages required by this package and their version bounds.
    --
    -- @
    -- Requires: libssl libcrypto
    -- @

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
    --
    -- @
    -- Cflags: -I\"${includedir}\"
    -- @

    , cflagsPrivate :: PkgTemplate
    -- ^ Compiler flags specific to this package and any required libraries
    -- that don't support /pkg-config/ for when linking statically. If the
    -- required libraries support /pkg-config/, they should be added to
    -- @Requires@ (@requires@ field) or @Requires.private@ (@requiresPrivate@
    -- field).
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @
    -- Cflags.private: -I"${includedir}" -DBUILD_STATIC
    -- @
    --
    -- @since 0.3.0.0

    , libs :: PkgTemplate
    -- ^ Linking flags specific to this package and any required libraries that
    -- don't support /pkg-config/. The same rules as for @Cflags@ (@cflags@
    -- field) apply here.
    --
    -- @
    -- Libs: -L\"${libdir}\" -lssh
    -- @

    , libsPrivate :: PkgTemplate
    -- ^ Linking flags for private libraries required by this package but not
    -- exposed to applications. The same rules as for @Cflags@ (@cflags@ field)
    -- apply here.
    --
    -- @
    -- Libs.private: -lz -lm
    -- @
    }
  deriving stock (Data, Generic, Show)

instance QuickCheck.Arbitrary PkgConfig where
    arbitrary :: QuickCheck.Gen PkgConfig
    arbitrary = do
        variables       <- QuickCheck.arbitrary
        name            <- QuickCheck.arbitrary
        description     <- QuickCheck.arbitrary
        url             <- QuickCheck.arbitrary
        version         <- QuickCheck.arbitrary
        requires        <- QuickCheck.arbitrary
        requiresPrivate <- QuickCheck.arbitrary
        conflicts       <- QuickCheck.arbitrary
        provides        <- QuickCheck.arbitrary
        cflags          <- QuickCheck.arbitrary
        cflagsPrivate   <- QuickCheck.arbitrary
        libs            <- QuickCheck.arbitrary
        libsPrivate     <- QuickCheck.arbitrary
        pure PkgConfig{..}

instance QuickCheck.CoArbitrary PkgConfig where
    coarbitrary :: PkgConfig -> QuickCheck.Gen b -> QuickCheck.Gen b
    coarbitrary PkgConfig{..} = QuickCheck.coarbitrary
        (  variables
        >< name
        >< description
        >< url
        >< version
        >< requires
        >< requiresPrivate
        >< conflicts
        >< provides
        >< cflags
        >< cflagsPrivate
        >< libs
        >< libsPrivate
        )
      where
        (><) = (,)

-- | Empty `PkgConfig` with all fields unset or set to an empty value.
--
-- >>> toStrictText empty
-- ""
-- >>> toStrictText empty{name = "foo"}
-- "Name: foo\n"
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

-- | Serialize `PkgConfig` in to strict `Strict.Text`.
--
-- >>> toStrictText empty{name = "foo", version = "1.2.3"}
-- "Name: foo\nVersion: 1.2.3\n"
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

-- | Lens to access variable definitions. See the @variables@ field of
-- `PkgConfig` for more information.
pkgVariables
    :: Functor f
    => ([PkgVariable] -> f [PkgVariable])
    -> PkgConfig -> f PkgConfig
pkgVariables f cfg@PkgConfig{variables = a} = f a <&> \b -> cfg{variables = b}

-- | Lens to access human-readable name of a library or package. See the @name@
-- field of `PkgConfig` for more information.
pkgName
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgName f cfg@PkgConfig{name = a} = f a <&> \b -> cfg{name = b}

-- | Lens to access brief description of the package. See the @description@
-- field of `PkgConfig` for more information.
pkgDescription
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgDescription f cfg@PkgConfig{description = a} =
    f a <&> \b -> cfg{description = b}

-- | Lens to access URL value where people can get more information about and
-- download the package. See the @url@ field of `PkgConfig` for more
-- information.
pkgUrl
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgUrl f cfg@PkgConfig{url = a} = f a <&> \b -> cfg{url = b}

-- | Lens to access version of the package. See the @version@ field of
-- `PkgConfig` for more information.
pkgVersion
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgVersion f cfg@PkgConfig{version = a} = f a <&> \b -> cfg{version = b}

-- | Lens to access list of packages required by this package and their version
-- bounds. See the @requires@ field of `PkgConfig` for more information.
pkgRequires
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequires f cfg@PkgConfig{requires = a} = f a <&> \b -> cfg{requires = b}

-- | Lens to access compiler flags specific to this package and any required
-- libraries that don't support /pkg-config/. See the @requiresPrivate@ field
-- of `PkgConfig` for more information.
pkgRequiresPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresPrivate f cfg@PkgConfig{requiresPrivate = a} =
    f a <&> \b -> cfg{requiresPrivate = b}

-- | Lens to access packages that this one conflicts with. See the @conflicts@
-- field of `PkgConfig` for more information.
pkgConflicts
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgConflicts f cfg@PkgConfig{conflicts = a} =
    f a <&> \b -> cfg{conflicts = b}

-- | Lens to access dependencies that the package fills and that may be
-- provided by an alternate package. See the @provides@ field of `PkgConfig`
-- for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgProvides
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgProvides f cfg@PkgConfig{provides = a} = f a <&> \b -> cfg{provides = b}

-- | Lens to access compiler flags specific to this package and any required
-- libraries that don't support /pkg-config/. See the @cflags@ field of
-- `PkgConfig` for more information.
pkgCflags
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgCflags f cfg@PkgConfig{cflags = a} = f a <&> \b -> cfg{cflags = b}

-- | Lens to access compiler flags specific to this package and any required
-- libraries that don't support /pkg-config/ for when linking statically. See
-- the @cflagsPrivate@ field of `PkgConfig` for more information.
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

-- | Lens to access linking flags specific to this package and any required
-- libraries that don't support /pkg-config/. See the @libs@ field of
-- `PkgConfig` for more information.
pkgLibs
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibs f cfg@PkgConfig{libs = a} = f a <&> \b -> cfg{libs = b}

-- | Lens to access linking flags for private libraries required by this
-- package but not exposed to applications. See the @libsPrivate@ field of
-- `PkgConfig` for more information.
pkgLibsPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibsPrivate f cfg@PkgConfig{libsPrivate = a} =
    f a <&> \b -> cfg{libsPrivate = b}
