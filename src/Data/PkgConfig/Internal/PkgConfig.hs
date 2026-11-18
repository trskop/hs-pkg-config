{-# LANGUAGE StrictData #-}
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
    , PkgMaintainer
    , PkgCopyright
    , PkgUrl
    , PkgSource
    , PkgVersion

    -- * Lenses
    , pkgVariables
    , pkgName
    , pkgVersion
    , pkgDescription
    , pkgMaintainer
    , pkgCopyright
    , pkgLicense
    , pkgLicenseFile
    , pkgUrl
    , pkgSource
    , pkgRequires
    , pkgRequiresInternal
    , pkgRequiresPrivate
    , pkgRequiresShared
    , pkgConflicts
    , pkgProvides
    , pkgCflags
    , pkgCflagsPrivate
    , pkgCflagsShared
    , pkgLibs
    , pkgLibsPrivate
    , pkgLibsShared
    , pkgLinkAbi

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

-- | The preferred contact for the maintainer.
type PkgMaintainer = Strict.Text

-- | URL where more package information can be obtained.
type PkgUrl = Strict.Text

-- | [System Package Data Exchange (SPDX)
-- ](https://en.wikipedia.org/wiki/Software_Package_Data_Exchange) download
-- location.
type PkgSource = Strict.Text

-- | Package version may use variable expansion and so it is represented by
-- `PkgConfig`.
type PkgVersion = PkgTemplate

type PkgCopyright = Strict.Text

-- | Representation of /pkg-config/ configuration file.
--
-- === See also:
--
-- * [@pc(5)@ manual page](https://man.archlinux.org/man/pc.5.en)
data PkgConfig = PkgConfig
    { variables :: [PkgVariable]
    -- ^ (__optional__) Variable definitions.
    --
    -- @
    -- prefix=\/usr
    -- libdir=${prefix}\/lib\/x86_64
    -- includedir=${prefix}\/include
    -- @

    , name :: PkgName
    -- ^ (__required__) Human-readable name for a library or package. This
    -- field is not used by /pkg-config/ tool for queries, because it uses
    -- @.pc@ file base name.
    --
    -- @
    -- Name: libsomething
    -- @

    , version :: PkgVersion
    -- ^ (__required__) Version of the package.
    --
    -- @
    -- Version: 1.0.11
    -- @

    , description :: PkgDescription
    -- ^ (__required__) Brief description of the package.
    --
    -- @
    -- Description: Some example library
    -- @

    , maintainer :: PkgMaintainer
    -- ^ (__optional__) The preferred contact for the maintainer.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , copyright :: PkgCopyright
    -- ^ (__optional__) A copyright statement.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , license :: PkgTemplate
    -- ^ (__optional__) [System Package Data Exchange (SPDX)
    -- ](https://en.wikipedia.org/wiki/Software_Package_Data_Exchange) license
    -- identifier(s). Licenses can be combined by operators @AND@ and @OR@, and
    -- grouping @(@, @)@.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , licenseFile :: PkgTemplate
    -- ^ (__optional__) An absolute file path where full license is stored of
    -- this package.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , url :: PkgUrl
    -- ^ (__required__) URL where more package information can be obtained and
    -- where the package can be downloaded.
    --
    -- @
    -- URL: https:\/\/git.example.tld\/org\/project
    -- @

    , source :: PkgSource
    -- ^ (__optional__) [System Package Data Exchange (SPDX)
    -- ](https://en.wikipedia.org/wiki/Software_Package_Data_Exchange) download
    -- location.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , requires :: PkgTemplate
    -- ^ (__optional__) List of packages required by this package and their
    -- version bounds.
    --
    -- @
    -- Requires: libssl libcrypto
    -- @

    , requiresInternal :: PkgTemplate
    -- ^ (__optional__) Dependencies required for static linking.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , requiresPrivate :: PkgTemplate
    -- ^ (__optional__) List of private packages required by this package but
    -- not exposed to applications. The version specific rules from the
    -- @Requires@ field also apply here.

    , requiresShared :: PkgTemplate
    -- ^ (__optional__) Dependencies that are only applied when performing
    -- dynamic linking.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , conflicts :: PkgTemplate
    -- ^ (__optional__) An optional field describing packages that this one
    -- conflicts with. The version specific rules from the Requires field also
    -- apply here. This field also takes multiple instances of the same
    -- package. E.g.:
    --
    -- @
    -- Conflicts: bar \< 1.2.3, bar \>= 1.3.0.
    -- @

    , provides :: PkgTemplate
    -- ^ (__optional__) Dependencies that the package fills and that may be
    -- provided by an alternate package.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , cflags :: PkgTemplate
    -- ^ (__optional__) Compiler flags specific to this package and any
    -- required libraries that don't support /pkg-config/. If the required
    -- libraries support /pkg-config/, they should be added to @Requires@
    -- (@requires@ field) or @Requires.private@ (@requiresPrivate@ field).
    --
    -- @
    -- Cflags: -I\"${includedir}\"
    -- @

    , cflagsPrivate :: PkgTemplate
    -- ^ (__optional__) Compiler flags specific to this package and any
    -- required libraries that don't support /pkg-config/ for when linking
    -- statically. If the required libraries support /pkg-config/, they should
    -- be added to @Requires@ (@requires@ field) or @Requires.private@
    -- (@requiresPrivate@ field).
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @
    -- Cflags.private: -I"${includedir}" -DBUILD_STATIC
    -- @
    --
    -- @since 0.3.0.0

    , cflagsShared :: PkgTemplate
    -- ^ (__optional__) Compiler flags required for dynamic libraries and
    -- dynamic linking.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , libs :: PkgTemplate
    -- ^ (__optional__) Linking flags specific to this package and any required
    -- libraries that don't support /pkg-config/. The same rules as for
    -- @Cflags@ (@cflags@ field) apply here.
    --
    -- @
    -- Libs: -L\"${libdir}\" -lssh
    -- @

    , libsPrivate :: PkgTemplate
    -- ^ (__optional__) Linking flags for private libraries required by this
    -- package but not exposed to applications. The same rules as for @Cflags@
    -- (@cflags@ field) apply here.
    --
    -- @
    -- Libs.private: -lz -lm
    -- @

    , libsShared :: PkgTemplate
    -- ^ (__optional__) Linking flags required for dynamic linking only.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0

    , linkAbi :: PkgTemplate
    -- ^ (__optional__) A comma-separated list of binary interfaces this
    -- package must be linked against.
    --
    -- This field is a /pkgconf/ extension.
    --
    -- @since 0.3.0.0
    }
  deriving stock (Data, Generic, Show)

instance QuickCheck.Arbitrary PkgConfig where
    arbitrary :: QuickCheck.Gen PkgConfig
    arbitrary = do
        variables        <- QuickCheck.arbitrary
        name             <- QuickCheck.arbitrary
        version          <- QuickCheck.arbitrary
        description      <- QuickCheck.arbitrary
        maintainer       <- QuickCheck.arbitrary
        copyright        <- QuickCheck.arbitrary
        license          <- QuickCheck.arbitrary
        licenseFile      <- QuickCheck.arbitrary
        url              <- QuickCheck.arbitrary
        source           <- QuickCheck.arbitrary
        requires         <- QuickCheck.arbitrary
        requiresInternal <- QuickCheck.arbitrary
        requiresPrivate  <- QuickCheck.arbitrary
        requiresShared   <- QuickCheck.arbitrary
        conflicts        <- QuickCheck.arbitrary
        provides         <- QuickCheck.arbitrary
        cflags           <- QuickCheck.arbitrary
        cflagsPrivate    <- QuickCheck.arbitrary
        cflagsShared     <- QuickCheck.arbitrary
        libs             <- QuickCheck.arbitrary
        libsPrivate      <- QuickCheck.arbitrary
        libsShared       <- QuickCheck.arbitrary
        linkAbi          <- QuickCheck.arbitrary
        pure PkgConfig{..}

instance QuickCheck.CoArbitrary PkgConfig where
    coarbitrary :: PkgConfig -> QuickCheck.Gen b -> QuickCheck.Gen b
    coarbitrary PkgConfig{..} = QuickCheck.coarbitrary
        (  variables
        >< name
        >< version
        >< description
        >< maintainer
        >< copyright
        >< license
        >< licenseFile
        >< url
        >< source
        >< requires
        >< requiresInternal
        >< requiresPrivate
        >< requiresShared
        >< conflicts
        >< provides
        >< cflags
        >< cflagsPrivate
        >< cflagsShared
        >< libs
        >< libsPrivate
        >< libsShared
        >< linkAbi
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
    , version = Template.empty
    , description = Strict.Text.empty
    , maintainer = Strict.Text.empty
    , copyright = Strict.Text.empty
    , license = Template.empty
    , licenseFile = Template.empty
    , url = Strict.Text.empty
    , source = Strict.Text.empty
    , requires = Template.empty
    , requiresInternal = Template.empty
    , requiresPrivate = Template.empty
    , requiresShared = Template.empty
    , conflicts = Template.empty
    , provides = Template.empty
    , cflags = Template.empty
    , cflagsPrivate = Template.empty
    , cflagsShared = Template.empty
    , libs = Template.empty
    , libsPrivate = Template.empty
    , libsShared = Template.empty
    , linkAbi = Template.empty
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
        [ "Name"              <:>  name
        , "Version"           <:>. version
        , "Description"       <:>  description
        , "Maintainer"        <:>  maintainer
        , "Copyright"         <:>  copyright
        , "License"           <:>. license
        , "LicenseFile"       <:>. licenseFile
        , "URL"               <:>  url
        , "Source"            <:>  source
        , "Requires"          <:>. requires
        , "Requires.internal" <:>. requiresInternal
        , "Requires.private"  <:>. requiresPrivate
        , "Requires.shared"   <:>. requiresShared
        , "Conflicts"         <:>. conflicts
        , "Provides"          <:>. provides
        , "Cflags"            <:>. cflags
        , "Cflags.private"    <:>. cflagsPrivate
        , "Cflags.shared"     <:>. cflagsShared
        , "Libs"              <:>. libs
        , "Libs.private"      <:>. libsPrivate
        , "Libs.shared"       <:>. libsShared
        , "Link.ABI"          <:>. linkAbi
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
    => (PkgName -> f PkgName)
    -> PkgConfig -> f PkgConfig
pkgName f cfg@PkgConfig{name = a} = f a <&> \b -> cfg{name = b}

-- | Lens to access version of the package. See the @version@ field of
-- `PkgConfig` for more information.
pkgVersion
    :: Functor f
    => (PkgVersion -> f PkgVersion)
    -> PkgConfig -> f PkgConfig
pkgVersion f cfg@PkgConfig{version = a} = f a <&> \b -> cfg{version = b}

-- | Lens to access brief description of the package. See the @description@
-- field of `PkgConfig` for more information.
pkgDescription
    :: Functor f
    => (PkgDescription -> f PkgDescription)
    -> PkgConfig -> f PkgConfig
pkgDescription f cfg@PkgConfig{description = a} =
    f a <&> \b -> cfg{description = b}

-- | Lens to access the preferred contact for the maintainer. See the
-- @maintainer@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgMaintainer
    :: Functor f
    => (PkgMaintainer -> f PkgMaintainer)
    -> PkgConfig -> f PkgConfig
pkgMaintainer f cfg@PkgConfig{maintainer = a} =
    f a <&> \b -> cfg{maintainer = b}

-- | Lens to access the copyright statement. See the @copyright@ field of
-- `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgCopyright
    :: Functor f
    => (PkgCopyright -> f PkgCopyright)
    -> PkgConfig -> f PkgConfig
pkgCopyright f cfg@PkgConfig{copyright = a} = f a <&> \b -> cfg{copyright = b}

-- | Lens to access [System Package Data Exchange (SPDX)
-- ](https://en.wikipedia.org/wiki/Software_Package_Data_Exchange) license
-- identifier(s). See the @license@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgLicense
    :: Functor f
    => (PkgVersion -> f PkgVersion)
    -> PkgConfig -> f PkgConfig
pkgLicense f cfg@PkgConfig{license = a} = f a <&> \b -> cfg{license = b}

-- | Lens to access an absolute file path where full license is stored of this
-- package. See the @licenseFile@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgLicenseFile
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLicenseFile f cfg@PkgConfig{licenseFile = a} =
    f a <&> \b -> cfg{licenseFile = b}

-- | Lens to access URL value where people can get more information about and
-- download the package. See the @url@ field of `PkgConfig` for more
-- information.
pkgUrl
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgUrl f cfg@PkgConfig{url = a} = f a <&> \b -> cfg{url = b}

-- | Lens ot access [System Package Data Exchange (SPDX)
-- ](https://en.wikipedia.org/wiki/Software_Package_Data_Exchange) download
-- location. See the @source@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgSource
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgConfig -> f PkgConfig
pkgSource f cfg@PkgConfig{url = a} = f a <&> \b -> cfg{source = b}

-- | Lens to access list of packages required by this package and their version
-- bounds. See the @requires@ field of `PkgConfig` for more information.
pkgRequires
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequires f cfg@PkgConfig{requires = a} = f a <&> \b -> cfg{requires = b}

-- | Lens to access dependencies required for static linking.
-- See the @requiresInternal@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgRequiresInternal
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresInternal f cfg@PkgConfig{requiresInternal = a} =
    f a <&> \b -> cfg{requiresInternal = b}

-- | Lens to access compiler flags specific to this package and any required
-- libraries that don't support /pkg-config/. See the @requiresPrivate@ field
-- of `PkgConfig` for more information.
pkgRequiresPrivate
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresPrivate f cfg@PkgConfig{requiresPrivate = a} =
    f a <&> \b -> cfg{requiresPrivate = b}

-- | Lens to access dependencies that are only applied when performing dynamic
-- linking. See the @requiresShared@ field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgRequiresShared
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgRequiresShared f cfg@PkgConfig{requiresShared = a} =
    f a <&> \b -> cfg{requiresShared = b}

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

-- | Lens to access compiler flags required for dynamic libraries and
-- dynamic linking. See the @cflagsShared@ field of `PkgConfig` for more
-- information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgCflagsShared
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgCflagsShared f cfg@PkgConfig{cflagsShared = a} =
    f a <&> \b -> cfg{cflagsShared = b}

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

-- | Lens to access linking flags for dynamic linking. See the @libsShared@
-- field of `PkgConfig` for more information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgLibsShared
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLibsShared f cfg@PkgConfig{libsShared = a} =
    f a <&> \b -> cfg{libsShared = b}

-- | Lens to access a comma-separated list of binary interfaces this package
-- must be linked against. See the @linkAbi@ field of `PkgConfig` for more
-- information.
--
-- This field is a /pkgconf/ extension.
--
-- @since 0.3.0.0
pkgLinkAbi
    :: Functor f
    => (PkgTemplate -> f PkgTemplate)
    -> PkgConfig -> f PkgConfig
pkgLinkAbi f cfg@PkgConfig{linkAbi = a} = f a <&> \b -> cfg{linkAbi = b}
