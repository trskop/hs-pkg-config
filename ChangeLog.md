# ChangeLog / ReleaseNotes


## Version 0.3.0.0

* Support `pkgconf` extensions.

* Simplify `PkgConfig` data type definitions so that its easier to use it
  directly (without lenses). This includes renaming of multiple functions to
  avoid conflicts. **breaking change**

* Drop `data-default-class` dependency. **breaking change**

* Introduces `VersionField` type class, `versionFields`, and `ver` functions
  that generalise version construction. In addition function `version` is now
  named `versionWord`. It was renamed due to conflict with `PkgConfig` field
  name. **breaking change**

* Fixed `instance Show PkgTemplate` to use string encoding. **breaking change**

* Introduces `package.yaml` as a primary source of truth instead of
  `hs-pkg-config.cabal`.

* Introduces `Eq` and `Show` instances for `Fragment` type for unit testing
  purposes.

* Initial set of unit tests.


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
