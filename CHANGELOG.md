# Revision history for hpc-sonarqube

## Support SonarQube instead of Codecov (SCB adapations) -- 2022-06-27
Modify coverage logic. Emit XML instead of JSON. Change module and
executable names.

## 0.3.0.0 -- 2021-04-02

Modify command-line argument to support TOOL:TEST_SUITE style target,
support searching .tix and .mix files made by cabal-install and
stack. Add options to customize the search for .mix and .tix files
with temporary directories made with cabal-install and stack.

Add new module ``Trace.Hpc.Codecov.Discover``. Rename module
``Trace.Hpc.Codecov.Error`` to ``Trace.Hpc.Codecov.Exception``.
Rename function ``main`` to ``defaultMain`` in
``Trace.Hpc.Codecov.Main``.

## 0.2.0.2 -- 2021-03-25

Minor modification to support ghc 9.0.1.

CI configuration update to manage git repository.

## 0.2.0.1 -- 2021-01-2

Minor package dependency updates.

## 0.1.0.0 -- 2020-02-08

Initial release.
