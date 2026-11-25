{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type describing pkg-config configuration file
-- Copyright:    (c) 2026 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- __TL;DR:__ Validation for /pkg-config/ file representation as implemented by
-- `PkgConfig` data type.
module Data.PkgConfig.Validate
    (
    -- $top-level

    -- * Validation
      ValidationError(..)
    , FieldReference(..)
    , validate
    )
  where

import Data.Eq (Eq)
import Data.Foldable (foldl')
import Data.Function ((.))
import Data.Functor ((<&>))
import Data.List as List (concat)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel(fromLabel))
import Text.Show (Show)

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
    ( empty
    , fromList
    , difference
    , singleton
    , toList
    , unions
    )
import Data.Either.Validation (Validation(Failure, Success), )
import Data.Text qualified as Strict (Text)
import Data.Text qualified as Strict.Text

import Data.PkgConfig.Internal.PkgConfig (PkgConfig, PkgVariable)
import Data.PkgConfig.Internal.PkgConfig qualified as PkgConfig
import Data.PkgConfig.Internal.Template (PkgTemplate)
import Data.PkgConfig.Internal.Template qualified as Template
    ( getVariables
    , null
    )


-- $top-level
--
-- #top-level#
--
-- Validation includes:
--
-- *   Check for __missing required fields__, see `PkgConfig` fields to see
--     which fields are required.
--
--     __Be aware__ that while some fields that considered required are
--     commonly ommited in practice. For example, @URL@ field is one of the
--     most commonly ommited fields. Because of this it is advised to consider
--     some of these validation errors as warnings instead of errors.
--
-- *   Check that all __defined variables are actually used__, see @variables@
--     field of `PkgConfig` data type.
--
--     __Be aware__ that, in practice, a lot of /pkg-config/ files contain
--     unused variable definitions. The main reason is that these files are
--     commonly generated using [GNU Autotools
--     ](https://en.wikipedia.org/wiki/GNU_Autotools). In addition these
--     variables can be used independently via /pkg-config/, /pkgconf/, or
--     other compatible tool. For example by invoking one of the following:
--
--     @
--     pkgconf --print-variables MODULE […]
--     pkgconf --variable=VARIABLE MODULE […]
--     pkg-cofnig --variable=VARIABLE MODULE […]
--     @
--
-- *   Check that all __variable expansions are referencing existing variables__.
--
--     __Be aware__ that while these are most commonly errors, it is possible
--     to define variables when invoking /pkg-config/, /pkgconf/, or similar
--     and fill missing values.
--
--     @
--     pkgconf --define-variable=NAME=VALUE MODULE […]
--     pkg-config --define-variable=NAME=VALUE MODULE […]
--     @

-- | Error describing possible issues in `PkgConfig`.
data ValidationError
    = MissingRequiredFieldError FieldReference
    -- ^ A field with a required value is empty.

    | UndefinedVariableError
        FieldReference{-field where the error was detected-}
        Strict.Text{-undefined variable name-}
        Strict.Text{-variable name where its used, can be ""-}
    -- ^ A variable is being used in the specified fields. If it is used in the
    -- variables section instead then the field names will be set to empty
    -- strings.

    | UnusedVariableError Strict.Text{-variable name-}
    -- ^ A variable is defined but not used.

  deriving stock (Eq, Generic, Ord, Show)

-- | References a /pkg-config/ field, both the field of `PkgConfig` data type
-- and the /pkg-cofnig/ file.
data FieldReference = FieldReference
    { dataTypeField :: Strict.Text
    -- ^ `PkgConfig` field name.

    , pcFileField :: Strict.Text
    -- ^ Field name as rendered in /pkg-config/ file as described in
    -- [@pc(5)@](https://man.archlinux.org/man/pc.5.en).
    --
    -- Value can be an empty string iff there is no corresponding field in
    -- /pkg-config/ file. For example, @variables@ field of `PkgConfig` is
    -- represented as a section with variable definitions and not a single
    -- field.
    }
  deriving stock (Eq, Generic, Ord, Show)

instance IsLabel "variables" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "variables" ""

instance IsLabel "name" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "name" "Name"

instance IsLabel "version" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "version" "Version"

instance IsLabel "description" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "description" "Description"

instance IsLabel "maintainer" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "maintainer" "Maintainer"

instance IsLabel "copyright" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "copyright" "Copyright"

instance IsLabel "license" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "license" "License"

instance IsLabel "licenseFile" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "licenseFile" "LicenseFile"

instance IsLabel "url" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "url" "URL"

instance IsLabel "source" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "source" "Source"

instance IsLabel "requires" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "requires" "Requires"

instance IsLabel "requiresInternal" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "requiresInternal" "Requires.internal"

instance IsLabel "requiresPrivate" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "requiresPrivate" "Requires.private"

instance IsLabel "requiresShared" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "requiresShared" "Requires.shared"

instance IsLabel "conflicts" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "conflicts" "Conflicts"

instance IsLabel "provides" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "provides" "Provides"

instance IsLabel "cflags" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "cflags" "Cflags"

instance IsLabel "cflagsPrivate" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "cflagsPrivate" "Cflags.private"

instance IsLabel "cflagsShared" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "cflagsShared" "Cflags.shared"

instance IsLabel "libs" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "libs" "Libs"

instance IsLabel "libsPrivate" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "libsPrivate" "Libs.private"

instance IsLabel "libsShared" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "libsShared" "Libs.shared"

instance IsLabel "linkAbi" FieldReference where
    fromLabel :: FieldReference
    fromLabel = FieldReference "linkAbi" "Link.ABI"

-- | Perform a basic validation of `PkgConfig` value.
validate :: PkgConfig -> Validation (NonEmpty ValidationError) PkgConfig
validate pkgcfg = case nonEmpty errors of
    Nothing -> Success pkgcfg
    Just es -> Failure es
  where
    errors :: [ValidationError]
    errors = List.concat
        [ [ MissingRequiredFieldError #name
          | Strict.Text.null pkgcfg.name
          ]
        , [ MissingRequiredFieldError #version
          | Template.null pkgcfg.version
          ]
        , [ MissingRequiredFieldError #description
          | Strict.Text.null pkgcfg.description
          ]
        , [ MissingRequiredFieldError #url
          | Strict.Text.null pkgcfg.url
          ]

        , variableErrors

        , undefVars #version          versionVars
        , undefVars #license          licenseVars
        , undefVars #licenseFile      licenseFileVars
        , undefVars #requires         requiresVars
        , undefVars #requiresInternal requiresInternalVars
        , undefVars #requiresPrivate  requiresPrivateVars
        , undefVars #requiresShared   requiresSharedVars
        , undefVars #conflicts        conflictsVars
        , undefVars #provides         providesVars
        , undefVars #cflags           cflagsVars
        , undefVars #cflagsPrivate    cflagsPrivateVars
        , undefVars #cflagsShared     cflagsSharedVars
        , undefVars #libs             libsVars
        , undefVars #libsPrivate      libsPrivateVars
        , undefVars #libsShared       libsSharedVars
        , undefVars #linkAbi          linkAbiVars

        , HashSet.toList (declaredVariables `HashSet.difference` usedVars)
            <&> UnusedVariableError
        ]

    variableErrors :: [ValidationError]
    declaredVariables :: HashSet Strict.Text
    usedVars :: HashSet Strict.Text
    (variableErrors, declaredVariables, usedVars) =
        let (errs, declared, used) =
                validateVariablesField pkgcfg.variables

            used' :: HashSet Strict.Text
            used' = HashSet.unions
                [ versionVars
                , licenseVars
                , licenseFileVars
                , requiresVars
                , requiresInternalVars
                , requiresPrivateVars
                , requiresSharedVars
                , conflictsVars
                , providesVars
                , cflagsVars
                , cflagsPrivateVars
                , cflagsSharedVars
                , libsVars
                , libsPrivateVars
                , libsSharedVars
                , linkAbiVars
                ]

        in  (errs, declared, used <> used')

    undefVars :: FieldReference -> HashSet Strict.Text -> [ValidationError]
    undefVars = undefinedVariables declaredVariables

    versionVars          = getVariables pkgcfg.version
    licenseVars          = getVariables pkgcfg.license
    licenseFileVars      = getVariables pkgcfg.licenseFile
    requiresVars         = getVariables pkgcfg.requires
    requiresInternalVars = getVariables pkgcfg.requiresInternal
    requiresPrivateVars  = getVariables pkgcfg.requiresPrivate
    requiresSharedVars   = getVariables pkgcfg.requiresShared
    conflictsVars        = getVariables pkgcfg.conflicts
    providesVars         = getVariables pkgcfg.provides
    cflagsVars           = getVariables pkgcfg.cflags
    cflagsPrivateVars    = getVariables pkgcfg.cflagsPrivate
    cflagsSharedVars     = getVariables pkgcfg.cflagsShared
    libsVars             = getVariables pkgcfg.libs
    libsPrivateVars      = getVariables pkgcfg.libsPrivate
    libsSharedVars       = getVariables pkgcfg.libsShared
    linkAbiVars          = getVariables pkgcfg.linkAbi

-- | __INTERNAL__ Validate variable declarations.
validateVariablesField
    :: [PkgVariable]
    -- ^ Variable declarations to validate.
    -> ([ValidationError], HashSet Strict.Text, HashSet Strict.Text)
    -- ^ The result can be described as:
    --
    -- @
    -- ( errors :: [`ValidationError`]
    -- , declaredVariables :: `HashSet` `Strict.Text`
    -- , usedVariables :: `HashSet` `Strict.Text`
    -- )
    -- @
    --
    -- Where:
    --
    -- [@errors@]:
    --     Errors encountered while validating variable defintions.
    --
    -- [@declaredVariables@]:
    --     Set of all variable names that were declared.
    --
    -- [@usedVariables@]:
    --     Set of all variables that were used in variable declarations.
validateVariablesField = foldl' go ([], HashSet.empty, HashSet.empty)
  where
    go  :: ([ValidationError], HashSet Strict.Text, HashSet Strict.Text)
        -> PkgVariable
        -> ([ValidationError], HashSet Strict.Text, HashSet Strict.Text)
    go (errs, accDefs, accUsed) (n, t) =
        let usedVars = getVariables t

            usedUndefVars = usedVars `HashSet.difference` accDefs

            errs' = HashSet.toList usedUndefVars <&> \v ->
                UndefinedVariableError #variables v n

        in (errs <> errs', accDefs <> HashSet.singleton n, accUsed <> usedVars)

-- | __INTERNAL__ Check for undefined variable names in a value of a specific
-- `PkgConfig` field.
undefinedVariables
    :: HashSet Strict.Text
    -> FieldReference
    -> HashSet Strict.Text
    -- ^ Variables used by the specific `PkgConfig` field.
    -> [ValidationError]
    -- ^ List of all detected validation errors.
undefinedVariables declared fieldRef x =
    HashSet.toList (x `HashSet.difference` declared) <&> \v ->
        UndefinedVariableError fieldRef v ""

-- | __INTERNAL__
getVariables :: PkgTemplate -> HashSet Strict.Text
getVariables = HashSet.fromList . Template.getVariables
