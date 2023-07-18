# Revision history for wai-middleware-delegate

`wai-middleware-delegate` uses [PVP Versioning][1].

## 0.1.4.0 -- 2023-17-11

* Relax the upper bounds on bytestring to allow bytestring 0.12.1
* Remove dependencies on http-conduit and connection
* Switch to running the integration tests using http-bin in docker

## 0.1.3.1 -- 2022-08-09

* Relax the version bounds so that pre 2.0 versions of text are still supported

## 0.1.3.0 -- 2022-08-09

* Bump a dependency - switch to text-2.0

## 0.1.2.4 -- 2021-09-30

* Fix cabal reference to data files

## 0.1.2.3 -- 2021-09-30

* Add missing data files needed by the tests

## 0.1.2.2 -- 2021-09-29

* Fix the module documentation

## 0.1.2.1 -- 2021-09-29

* Relax some dependency upper-bounds
* Compile with a later version of GHC


## 0.1.2.0 -- 2020-10-20

* Relax some dependency upper-bounds


## 0.1.1.0 -- 2018-08-06

* Add configuration for the number of redirects


## 0.1.0.0 -- 2018-07-28

* Initial version.

[1]: https://pvp.haskell.org
