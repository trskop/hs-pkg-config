# ChangeLog / ReleaseNotes


## Version 0.3.0.0

*   Support all known `pkgconf` extensions.

*   Simplify `PkgConfig` data type definitions so that its easier to use it
    directly (without lenses). This includes renaming of multiple functions to
    avoid conflicts. (**breaking change**)

*   Drop `data-default-class` dependency. (**breaking change**)

*   Introduces `VersionField` type class, `versionFields`, and `ver` functions
    that generalise version construction. In addition function `version` is now
    named `versionWord`. It was renamed due to conflict with `PkgConfig` field
    name. (**breaking change**)

*   Fixed `instance Show PkgTemplate` to use string encoding.
    (**breaking change**)

*   Introduces `package.yaml` as a primary source of truth instead of
    `hs-pkg-config.cabal`. The generated cabal file is included making this
    backward compatible.

*   Introduces `Eq` and `Show` instances for `Fragment` type for unit testing
    purposes.

    Both instances are structural, in other words, they follow the structure of
    the `Fragment` data type. For context, `Eq` and `Show` instances for
    `PkgTemplate` are not structural, they work on its string encoding.

*   Introduces `hPutPkgConfig` and generalises `writePkgConfig`:

    ```haskell
    Data.PkgCofig.writePkgConfig :: MonadIO io => FilePath -> PkgConfig -> io ()
    Data.PkgCofig.hPutPkgConfig :: MonadIO io => Handle -> PkgConfig -> io ()
    ```

    The reason for using `MonadIO` is that this library is very likely to be
    used from within a monadic stack.

*   Handle empty values in `separatedBy`, `include`, `libraries`, and
    `libraryPath` by skipping them. (**breaking change**)

    ```haskell
    separatedBy [mempty] = separatedBy [] = mempty
    separatedBy [mempty, x, mempty, y] = separatedBy [x, y]
    ```

    Same applies to the other aforementioned functions.

    Unit tests for these properties included.

*   Function `</>` and `<.>` handle empty value in the same way as the
    `filepath` library. (**breaking change**)

    ```haskell
    mempty </> x = x
    x </> mempty = x
    ```

    ```haskell
    mempty <.> x = "." <> x
    x <.> mempty = x
    ```

    Unit tests for these properties included.

*   Functions `~=`, `~/=`, `~<`, `~>`, `~<=`, and `~>=` now handle empty values
    in a sensible way. For example:

    ```haskell
    "" ~= [1, 2, 3] = mempty
    "foo" ~= []     = pkg
    ```

    Unit tests for these properties included.

*   Introduces the following functions:

    ```haskell
    (Data.PkgCofig..=) :: Strict.Text -> PkgTemplate -> PkgVariable
    ```

    ```haskell
    Data.PkgCofig.litWith, Data.PkgCofig.varWith
        :: (a -> Strict.Text) -> a -> PkgTemplate
    ```

    ```haskell
    Data.PkgCofig.Internal.Template.null :: PkgTemplate -> Bool
    ```

*   QuickCheck's `Arbitrary` and `CoArbitrary` instances for `PkgConfig`
    and `PkgTemplate`

*   Unit tests for some existing code as well as some of the code introduced in
    this version.


## Version 0.2.1.1

* Minor code cleanup.


## Version 0.2.1.0

* Derive `Generic` instance for `PkgConfig`.

* Introduced functions:
  * `versionInt :: [Int] -> PkgTemplate`
  * `toString :: PkgConfig -> String`

* Building option `-fpedantic`.

* Documentation updates.

* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/hs-pkg-config-0.2.1.0>


## Version 0.2.0.0

* New functions in `Data.PkgCofig` module:
  * `quote :: PkgTemplate -> PkgTemplate`
  * `option :: Strict.Text -> PkgTemplate -> PkgTemplate`
  * `strOption :: String -> PkgTemplate -> PkgTemplate`

* Quoting option arguments by default. This affects following functions from
  `Data.PkgCofig` module:
  * `includes :: [PkgTemplate] -> PkgTemplate`
  * `libraryPath :: [PkgTemplate] -> PkgTemplate`

* Lot of documentation updates.

* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/hs-pkg-config-0.2.0.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/hs-pkg-config-0.1.0.0>


[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
