# orgstat

[![Build Status](https://travis-ci.org/volhovm/orgstat.svg?branch=master)](https://travis-ci.org/volhovM/orgstat)
[![Hackage status](https://img.shields.io/hackage/v/orgstat.svg)](http://hackage.haskell.org/package/orgstat)

Orgstat is a statistics visualizer tool for org-mode. Given a set of org-mode files (gpg supported!),
it parses their AST, applies modifications such as tag filtering, pruning or selecting a subtree (which yields a _report_) and generates an _output_ of a specific type. Currently supported output types are:
* Timeline output: an SVG image that plots time intervals as horizontal bars on a vertical day-bar, for every day of the selected report range (e.g. for a week).
* Summary output: for a template string, `orgstat` replaces each `%reportName%` substring in it by the duration spent on the corresponding report. This is useful for outputting progress info into the status bar, e.g. to keep track of number of hours spent on work today/this week.
* Script output: a generalization of summary output. Instead of outputting a single string with substitutions, selected reports' durations are set as the environment variables and passed to the user-specified script that is run in this new environment.
* Block output: similar to the default `org-clock-report`, though formatting is more similar to one that `tree` Unix utility provides. I am using it primarily for debugging.

## Building/installing

`orgstat` uses haskell build tool [stack](https://docs.haskellstack.org/en/stable/README/). In order to build the project, run `stack build` in the project directory. Then, `stack exec orgstat -- --help` is the way to run the executable.

Since `orgstat` is also available on hackage, you can use `cabal install orgstat` to obtain it. If you are using `nix` package manager you can find `orgstat` in `nixpkgs` as `haskellPackages.orgstat` since `nixpkgs` has effectively everything available on hackage directly.

To install `orgstat` with `nix`:
```
nix-env -f "<nixpkgs>" -iA haskellPackages.orgstat
```

## Running

Check out [orgstatExample.yaml](./orgstatExample.yaml) sample configuration file (or my [personal](https://github.com/volhovm/dotfiles/blob/master/config/.orgstat.yaml) config) and `orgstat --help`:
```
$> orgstat --help
----- OrgStat ------

Usage: orgstat [--version] [--help] [--conf-path FILEPATH] [--debug]
               [--xdg-open] [--output|--select-output ARG]
               [--output-dir FILEPATH]
  Statistic reports visualizer for org-mode

Available options:
  --version                Show version
  --help                   Show this help text
  --conf-path FILEPATH     Path to the configuration file
  --debug                  Enable debug logging
  --xdg-open               Open each report using xdg-open
  --output,--select-output ARG
                           Output name(s) you want to process (default: all
                           outputs are processed)
  --output-dir FILEPATH    Final output directory that overrides one in config.
                           No extra subdirectories will be created
```
## Examples

See the [orgstatExample.yaml](./orgstatExample.yaml) configuration file.

Here is how a timeline report output looks like:
![Orgstat timeline report example](https://raw.githubusercontent.com/volhovM/orgstat/master/example.png)

To use `script` type output to put current progress reports into `xmobar`:
```
# all the reports are defined with a week range and a single filterbytag modifier with an appropriate tag
- name: curWeekStats
  type: summary
  template: "%thisWeekWork%/%thisWeekStudy% %thisWeekA1%/%thisWeekA2%/%thisWeekA3% %thisWeekI%/%thisWeekE%"
```

Then by running `stack exec orgstat -- --select-output resolveOutput --output-dir ~/`, `orgstat` puts the report into `~/curWeekStats.txt` yielding something like `0:57/0:09 2:03/2:48/3:16 1:57/2:34` inside. Next add a bit of cron and an xmobar task to read the text out of this file.

Alternatively, it is possible to use a more involved `script` type output, which passes all the relevant report length variables to the interpreter program, via environment variables. It is configured in the following way:
```
- name: thisWeekStatsScript
  type: script
  interpreter: "/bin/env sh"
  scriptPath: ~/dotfiles/scripts/orgstat_format_bar.sh
  reports: [thisWeekM,thisWeekH,thisWeekA,thisWeekE,todayM,todayH,todayA,todayE]
```
For the report `x`, `orgstat` will inject variable `x` containing the sum of intervals in the report (this is useful to summarise how many time was spent on a particular task category), and additionally it will inject a `xDurationsList` variable containing the list of all durations (which I personally use to count all 25min-long intervals, in a pomodoro-like fashion).
For the interpreter script example, see [my dotfiles](https://github.com/volhovm/dotfiles/blob/master/scripts/orgstat_format_bar.sh).

## Archiving tool

The executable `orgstatarch` provides a simple script that accepts a file as an input, and splits it in two according to the date provided.
The old file is an archive, and the new file is intended to be a refreshed version of the file passed.
The main difference from the default archiving tools org-mode provides is that `orgstatarch` tries to keep the current tree structure intact; that is, the archive structure is the same as the structure of the original file.
In particular, for the tasks that are never marked as done, but only accumulate logging intervals, `orgstatarch` will put two versions of this task in both files, with intervals split correspondingly.
This is quite helpful to get rid of massive `LOGBOOK` drawers.

The tool is highly work-in-progress (e.g. it assumes the specific output formatting, so one might want to apply `org-indent-region` on the outputs of `orgstatarch`), but it nevertheless illustrates how simple writing scripts with `orgstat` and `orgmode-parse` can be.

## Bugs and issues

If you experience any problems with the application, you can use `block` output and `--debug` to see, first of all, whether the org file is parsed correctly. And, of course, feel free to leave issues.
