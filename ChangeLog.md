# ChangeLog / ReleaseNotes


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
