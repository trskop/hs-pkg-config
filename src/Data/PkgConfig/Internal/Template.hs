{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Simple string template used by pkg-config
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Simple string template used by /pkg-config/.
module Data.PkgConfig.Internal.Template
    (
    -- * Template
      PkgTemplate
    , Template(..)
    , Fragment(..)

    -- * Smart Constructors
    , var
    , lit
    , strLit
    , singletonLit

    -- * Serialize

    -- ** Serialize Template
    , toLazyText
    , toStrictText
    , toTextBuilder

    -- ** Serialize Fragment
    , fragmentToBuilder
    , fragmentToStrictText

    -- * Query Template
    , variables
    )
  where

import Data.Bool (otherwise)
import Data.Char (Char)
import Data.Data (Data)
import Data.Eq (Eq((==)))
import Data.Foldable (Foldable(foldMap))
import Data.Function ((.), ($), on)
import Data.List as List ((++), map)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.String (IsString(fromString), String)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(show))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder


-- {{{ Template Definition ----------------------------------------------------

data Fragment
    = Literal  {-# UNPACK #-} !Strict.Text
    | Variable {-# UNPACK #-} !Strict.Text
  deriving (Data, Eq, Generic, Typeable)

newtype Template = Template [Fragment]
  deriving (Data, Generic, Typeable)

type PkgTemplate = Template

-- | Serialize fragment in to strict 'Strict.Text'.
fragmentToStrictText :: Fragment -> Strict.Text
fragmentToStrictText frag = case frag of
    Literal txt   -> txt
    Variable name -> Strict.Text.pack "${" <> name <> Strict.Text.singleton '}'

-- | Serialize fragment in to 'Text.Builder'.
fragmentToBuilder :: Fragment -> Text.Builder
fragmentToBuilder = Text.Builder.fromText . fragmentToStrictText

-- | Serialize template in to 'Text.Builder'.
toTextBuilder :: Template -> Text.Builder
toTextBuilder (Template fragments) = foldMap fragmentToBuilder fragments

-- | Serialize template in to lazy 'Lazy.Text'.
toLazyText :: Template -> Lazy.Text
toLazyText = Text.Builder.toLazyText . toTextBuilder

-- | Serialize template in to strict 'Strict.Text'.
toStrictText :: Template -> Strict.Text
toStrictText (Template fragments) =
    Strict.Text.concat $ List.map fragmentToStrictText fragments

-- {{{ Instances for Template -------------------------------------------------

-- | Requires template to be converted in to lazy string.
instance Eq Template where
    (==) = (==) `on` toLazyText

instance Monoid Template where
    mempty = Template []
    {-# INLINE mempty #-}

    Template fs `mappend` Template fs' = Template $ fs ++ fs'

instance Show Template where
    show = Strict.Text.unpack . toStrictText

instance IsString Template where
    fromString = strLit

-- }}} Instances for Template -------------------------------------------------

-- {{{ Smart Constructors -----------------------------------------------------

-- | Construct variable fragment of template.
--
-- >>> var "prefix" <> lit "/bin"
-- $prefix/bin
var :: Strict.Text -> PkgTemplate
var v = Template [Variable v]
{-# INLINE var #-}

-- | Construct literal fragment of template. This is useful if language
-- extension /OverloadedStrings/ is not enabled.
--
-- >>> var "prefix" <> lit "/bin"
-- $prefix/bin
lit :: Strict.Text -> PkgTemplate
lit l
  | Strict.Text.null l = mempty
  | otherwise          = Template [Literal l]

strLit :: String -> PkgTemplate
strLit = lit . Strict.Text.pack

singletonLit :: Char -> PkgTemplate
singletonLit = lit . Strict.Text.singleton

-- }}} Smart Constructors -----------------------------------------------------

-- {{{ Query Template ---------------------------------------------------------

variables :: PkgTemplate -> [Strict.Text]
variables (Template fragments) = variables' fragments
  where
    variables' []                = []
    variables' (x : xs)          = case x of
        Literal _  -> variables' xs
        Variable v -> v : variables' xs

-- }}} Query Template ---------------------------------------------------------

