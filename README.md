# orgstat

[![Build Status](https://travis-ci.org/volhovM/orgstat.svg?branch=master)](https://travis-ci.org/volhovM/orgstat)

Orgstat is statistics visualizer for org-mode. Given a set of org-mode files, 
it parses them, generates tree, applies modifications and generates reports
using specified params. Currently only timeline report type is supported, but
it's pretty easy to add txt-generating report type, contributions are welcomed.

For more details, check `orgstatExample.yaml` configuration file and `orgstat --help`:
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
