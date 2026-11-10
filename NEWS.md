# bugphyzz 1.8.0

## SIGNIFICANT USER-VISIBLE CHANGES

+ `importBugphyzz()` defaults to data release v1.1.0
  (10.5281/zenodo.24114812): 466,663 annotations for 93,838 taxa,
  33 attributes and 18 sources.
+ Imputed annotations are kept or dropped per attribute, by the lower 95%
  bound of a distance-matched Cohen's kappa, or R2 for numeric
  attributes (#292).
+ Discrete signature names start with `bugphyzz:`, not `bugphyz:` (#256).
+ `excludeRarely` and `v` are deprecated; use `asrScoreThreshold`,
  `kappaThreshold` and `r2Threshold` (#271).
+ Levi Waldron is the maintainer (#257).

## NEW FEATURES

+ `physiologies()` also reads the scripted sources in bugphyzzExports
  `sources/`: the 2026 Bergey's Manual extraction, Madin et al. 2020,
  BacDive, The Microbe Directory and NCBI RefSeq assemblies (#275).
+ New attribute `gc content` and new shape value `vibrio`.
+ `attribute_synonyms.tsv` maps source terms to the controlled
  vocabulary (#264).
+ New arguments `kappaThreshold`, `r2Threshold` and `asrScoreThreshold`;
  a threshold below its default also loads the imputations the export
  left out (#292).
+ Validation scores per attribute and per state ship as
  `validation_attributes.tsv` and `validation_states.tsv`.

## BUG FIXES AND MINOR IMPROVEMENTS

+ Cell length and width no longer collapse to the lower bound of their
  range (#275).
+ Downloads are cached per version.
+ Typos and missing terms in `attributes.tsv` are fixed (#281).
+ Tests that download data skip when offline (#266).

# bugphyzz 0.99.3
+ Update table with validations results in extdata to version 1.0.2

# bugphyzz 0.99.0
+ First version of the package for submission in Bioconductor.
