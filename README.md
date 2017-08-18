# orgstat

[![Build Status](https://travis-ci.org/volhovM/orgstat.svg?branch=master)](https://travis-ci.org/volhovM/orgstat)
[![Hackage status](https://img.shields.io/hackage/v/orgstat.svg)](http://hackage.haskell.org/package/orgstat)

Orgstat is statistics visualizer for org-mode. Given a set of org-mode files (gpg supported), 
it parses them, generates tree, applies modifications and generates reports
using specified params. Currently only timeline report type is implemented, but
it's pretty easy to add txt-generating report type and so on; contributions are welcomed.

## Building/installing

`orgstat` uses haskell build tool [stack](https://docs.haskellstack.org/en/stable/README/). In order to build the project, run `stack build` in the project directory. 
Since `orgstat` is also available on hackage, you can use `cabal install orgstat` to get it. If you're using `nix` package manager you can find `orgstat` in `nixpkgs` as `haskellPackages.orgstat` since `nixpkgs` has effectively everything available on hackage directly.

## Running

Check out `orgstatExample.yaml` configuration file (config is used to parametrize report) and `orgstat --help`:
```
ξ> stack exec orgstat -- --help
----- OrgStat ------

Usage: orgstat [--version] [--help] [--conf-path FILEPATH] [--xdg-open]
               [--debug]
  Statistic reports visualizer for org-mode

Available options:
  --version                Show version
  --help                   Show this help text
  --conf-path FILEPATH     Path to the configuration file
  --xdg-open               Open each report using xdg-open
  --debug                  Enable debug logging
```

Here how timeline report output looks like:
![Orgstat timeline report example](https://raw.githubusercontent.com/volhovM/orgstat/master/example.png)
