-- |
-- Module:       $HEADER$
-- Description:  Simple string template used by pkg-config
-- Copyright:    (c) 2014, 2017, 2026 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- __TL;DR:__ Simple string template used by /pkg-config/.
module Data.PkgConfig.Internal.Template
    (
    -- * Template
      PkgTemplate
    , Template(..)
    , Fragment(..)

    -- * Smart Constructors
    , empty
    , var
    , strVar
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
    , getVariables
    )
  where

import Data.Bool (Bool, (||), otherwise)
import Data.Char (Char)
import Data.Data (Data)
import Data.Eq (Eq((==)))
import Data.Foldable (Foldable(foldMap))
import Data.Function ((.), on)
import Data.Int (Int)
import Data.List as List (map)
import Data.Monoid (Monoid(mempty))
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString), String)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), ShowS)

import Data.Text qualified as Strict (Text)
import Data.Text qualified as Strict.Text
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder


-- {{{ Template Definition ----------------------------------------------------

-- | 'Template' fragment ca be either literal or variable. Literals are subject
-- to escaping rules when serialized.
data Fragment
    = Literal  {-# UNPACK #-} !Strict.Text
    | Variable {-# UNPACK #-} !Strict.Text
  deriving stock (Data, Eq, Generic, Show)

-- | 'Template' is a possibly empty sequence of fragments represented by
-- 'Fragment' data type.
newtype Template = Template [Fragment]
  deriving stock (Data, Generic)

-- | Template consists of variables and literal strings. All special characters
-- (@\'$\'@, @\'#\'@, @\'\\\'@ and end-of-line sequences) contained in literals
-- are escaped when serialized.
type PkgTemplate = Template

-- | Serialize fragment in to strict 'Strict.Text'. For literals function
-- performs escaping of special characters.
fragmentToStrictText :: Fragment -> Strict.Text
fragmentToStrictText = \case
    Literal txt   -> escape txt
    Variable name -> Strict.Text.pack "${" <> name <> Strict.Text.singleton '}'
  where
    -- There are two types of escaping in pkg-config. One is done by
    -- read_one_line() function, that uses '\' as escape character and then
    -- there is trim_and_sub() that treats sequence of two \'\$\' characters as
    -- just one '$'and doesn't perform variable expansion. Both mentioned
    -- functions can be found in "parse.c" file.

    -- Escape all special characters including end-of-line sequences.
    escape :: Strict.Text -> Strict.Text
    escape = Strict.Text.concat . escapeLoop

    -- Escape all special characters except end-of-line sequence.
    escapeChar :: Char -> Strict.Text
    escapeChar = Strict.Text.pack . \case
        '$'  -> "$$"
        '#'  -> "\\#"
        '\\' -> "\\\\"
        c    -> [c]

    -- Process text by splitting it on EOL, repeatedly, and escape special
    -- characters and end of line sequences.
    escapeLoop :: Strict.Text -> [Strict.Text]
    escapeLoop txt
      | Strict.Text.null txt = []
      | otherwise            =
        Strict.Text.concatMap escapeChar txt1 : if Strict.Text.null txt2
            then []
            else let (eol, txtRest) = processEol txt2
                in (backslash <> eol) : escapeLoop txtRest
      where
        (txt1, txt2) = Strict.Text.break isCrOrLf txt

    -- Function takes text and splits it to pair where first element is EOL
    -- character sequence, i.e. one of "\r", "\n", "\r\n", or "\n\r". Reason
    -- for this is that pkg-config implementation treats all this sequences as
    -- end of line character sequence.
    --
    -- Input condition:
    --   Text passed to this funtion starts with either '\r' or '\n'.
    processEol :: Strict.Text -> (Strict.Text, Strict.Text)
    processEol txt
      | (c1, c2) == (cr, lf) || (c1, c2) == (lf, cr) = (eol, txt')
        -- Value of eol is either "\r\n" or "\n\r". In either case it is
        -- escaped the same way, since pkg-config treats "\r\n" and "\n\r" both
        -- as single line terminator.
      | otherwise = (c1, c2 <> txt')
        -- There might be two cases here:
        --
        -- - Both "\r\r" and "\n\n" are two subsequent line terminators, but at
        --   the moment it is not possible to know if the later line terminator
        --   is not in fact "\r\n" or "\n\r" sequence.
        --
        -- - Its either '\r' or '\n' followed by non-eol character.
      where
        -- End of line character sequence can be at most 2 characters long
        -- ("\r\n" or "\n\r").
        (eol, txt') = Strict.Text.splitAt 2 txt
        (c1, c2) = Strict.Text.splitAt 1 eol

    isCrOrLf :: Char -> Bool
    isCrOrLf c = c == '\r' || c == '\n'

    backslash, cr, lf :: Strict.Text
    backslash = Strict.Text.singleton '\\'
    cr = Strict.Text.singleton '\r'
    lf = Strict.Text.singleton '\r'

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
    Strict.Text.concat (List.map fragmentToStrictText fragments)

-- {{{ Instances for Template -------------------------------------------------

-- | Requires template to be converted into lazy 'Lazy.Text'.
instance Eq Template where
    (==) :: Template -> Template -> Bool
    (==) = (==) `on` toLazyText

instance Semigroup Template where
    (<>) :: Template -> Template -> Template
    Template fs <> Template fs' = Template (fs <> fs')

-- | @`mempty` ≡ `empty`@
instance Monoid Template where
    mempty :: Template
    mempty = Template []
    {-# INLINE mempty #-}

instance Show Template where
    showsPrec :: Int -> Template -> ShowS
    showsPrec n = showsPrec n . toStrictText

-- | Construct a literal, see `strLit` for details.
instance IsString Template where
    fromString :: String -> Template
    fromString = strLit

-- }}} Instances for Template -------------------------------------------------

-- {{{ Smart Constructors -----------------------------------------------------

-- | Empty template.
--
-- === Properties:
--
-- @
-- `mempty` ≡ empty
-- @
empty :: PkgTemplate
empty = Template []

-- | Construct variable fragment of a template.
--
-- >>> var "prefix" <> lit "/bin"
-- "${prefix}/bin"
--
-- === Properties:
--
-- @
-- `var` \"\" ≡ `empty`
-- @
var :: Strict.Text -> PkgTemplate
var v
  | Strict.Text.null v = empty
  | otherwise          = Template [Variable v]

-- | Construct 'PkgTemplate' variable from 'String' by packing it in to strict
-- 'Strict.Text' first.
strVar :: String -> PkgTemplate
strVar = var . Strict.Text.pack

-- | Construct literal fragment of a template. This is useful if language
-- extension @OverloadedStrings@ is not enabled.
--
-- >>> var "prefix" <> lit "/bin"
-- "$prefix/bin"
--
-- >>> lit "${foo}"
-- "$${foo}"
--
-- === Properties:
--
-- @
-- `lit` \"\" ≡ `empty`
-- @
lit :: Strict.Text -> PkgTemplate
lit l
  | Strict.Text.null l = empty
  | otherwise          = Template [Literal l]

-- | Create 'PkgTemplate' literal from 'String' by packing it in to strict
-- 'Strict.Text' first.
strLit :: String -> PkgTemplate
strLit = lit . Strict.Text.pack

-- | Crate one character long 'PkgTemplate' literal.
singletonLit :: Char -> PkgTemplate
singletonLit = lit . Strict.Text.singleton

-- }}} Smart Constructors -----------------------------------------------------

-- {{{ Query Template ---------------------------------------------------------

-- | List all variables mentioned in 'PkgTemplate'.
--
-- >>> getVariables $ var "foo" </> "bar" </> var "baz"
-- ["foo","baz"]
--
-- === Notes:
--
-- Prior to version 0.3.0.0 this function was named @variables@, however, that
-- now conflicts with the `PkgConfig` field of the same name.
--
-- @since 0.3.0.0
getVariables :: PkgTemplate -> [Strict.Text]
getVariables (Template fragments) = go fragments
  where
    go = \case
        []     -> []
        x : xs -> case x of
            Literal{}  -> go xs
            Variable v -> v : go xs

-- }}} Query Template ---------------------------------------------------------
