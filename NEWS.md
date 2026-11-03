# bugphyzz 1.7.3
+ `importBugphyzz()` now filters imputed (ASR) annotations with separate
  thresholds for discrete and numeric attributes: `mccThreshold` (default 0.7)
  and `r2Threshold` (default 0.5), the values used in the bugphyzz manuscript.
  The old single `v` argument (default 0.8, documented as 0.5) is deprecated.
  With the new defaults, imputed genome size, coding genes and growth
  temperature are included again.
+ `excludeRarely = TRUE` now also drops annotations with Frequency "never", as
  the documentation already said.

# bugphyzz 1.7.2
+ Levi Waldron is now the maintainer.

# bugphyzz 0.99.3
+ Update table with validations results in extdata to version 1.0.2

# bugphyzz 0.99.0
+ First version of the package for submission in Bioconductor.
