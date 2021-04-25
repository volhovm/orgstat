0.1.10
==========
* Compatibility with base-4.14

0.1.9
==========
* Introduced passing the list of report durations into the script type output.
* Added a simple archiving tool.
* Fixed a small bug with `mergeClocks` which was causing multiple irrelevant tasks to inter-merge improperly.
* Improved README.

0.1.8
==========
* Switched to ghc 8.8

0.1.7
==========
* Switch "script" output interpreter from "sh" to "/bin/env sh".
* Update universum and orgmode-parse version (includes
  https://github.com/ixmatus/orgmode-parse/pull/53)

0.1.6
==========
* Support "or" tags -- now it's possible to specify "t1 | t2 | t3" as a modifier by
  providing a list of tags in config.
* Support of file-level tags: "#+FILETAGS: :tagA:tagB:tagC:" is correctly parsed
  and tags are propagated to all the file tree headers.
* Added new output type -- script output.
* Improved documentation, in particular in the orgstatExample.yaml.
* Breaking: Configuration changes:
  * Simplified the configuration, now there's only one default timeline config,
    which cannot be overridden on a per-output basis.
  * Config parameter `colorSalt` was removed, use `timelineDefault.colorSalt` instead.
  * Timeline default color is now white.
  * timelineDefault renamed into timelineParams.

0.1.5
=====

* Support selecting multiple outputs in cli: --select-output can be used several times,
  also it's renamed into --output.
* Remove log-warper dependency replacing with simple local logging.
* Update deps: universum-1.4.0, fmt-0.6, orgmode-parse-0.2.2, also update lts to 12.5

0.1.4
=====

* Updated orgmode-parse which fixes parsing issue: https://github.com/ixmatus/orgmode-parse/issues/35

0.1.3
=====

* Updated universum to 1.7.1, universum to 0.8.0. Switched to nightly/ghc-8.2.1.


0.1.2
=====

* Add pretty block output.

0.1.1
=====

* Introduce concise report feature.
* Change report hierarchy: separate reports from outputs.
* Fix minor bugs.

0.0.4
=====

* Update universum to 0.5.1.1
* Update log-warper to 1.1.4

0.0.3
=====

* Update universum/log-warper dependencies.

0.0.2
=====

* Add support for log-waprer 0.4.*

0.0.1
=====

* Initial release having timeline report type only.
