# bugphyzz 1.9.1

## SIGNIFICANT USER-VISIBLE CHANGES

+ ProTraits goes from confidence `medium` to `low`: its values are
  predictions from text mining and comparative genomics (Brbić et al.
  2016), and the resource is no longer maintained
  (waldronlab/bugphyzzExports#66, @lwaldron).

# bugphyzz 1.8.0

## SIGNIFICANT USER-VISIBLE CHANGES

+ `importBugphyzz()` defaults to data release v1.2.0
  (10.5281/zenodo.24166279): 477,741 annotations for 93,900 taxa,
  36 attributes and 19 sources.
+ The data are licensed CC BY-SA 4.0 from release v1.2.0, because they
  include annotations derived from Wikipedia; earlier releases are
  CC BY 4.0. The package stays Artistic-2.0.
+ Imputed annotations are kept or dropped per attribute, by the lower 95%
  bound of a distance-matched Cohen's kappa, or R2 for numeric
  attributes (#292, @lwaldron).
+ Discrete signature names start with `bugphyzz:`, not `bugphyz:`
  (#256, @lwaldron).
+ `excludeRarely` and `v` are deprecated; use `asrScoreThreshold`,
  `kappaThreshold` and `r2Threshold` (#271, @lwaldron).

## NEW FEATURES

+ `physiologies()` also reads the scripted sources in bugphyzzExports
  `sources/`: the 2026 Bergey's Manual extraction, Madin et al. 2020,
  BacDive, The Microbe Directory and NCBI RefSeq assemblies (#275, @lwaldron).
+ New attribute `gc content` and new shape value `vibrio`.
+ New source `Wikipedia_2026` (confidence medium, evidence nas): traits
  stated in English Wikipedia introductions, with the new attributes
  `halophily class`, `temperature class` and `ph class`; `skin` is also a
  habitat (#296, @lwaldron).
+ `attribute_synonyms.tsv` maps source terms to the controlled
  vocabulary (#264, @lwaldron).
+ New arguments `kappaThreshold`, `r2Threshold` and `asrScoreThreshold`;
  a threshold below its default also loads the imputations the export
  left out (#292, @lwaldron).
+ Validation scores per attribute and per state ship as
  `validation_attributes.tsv` and `validation_states.tsv`.
+ New antimicrobial resistance attributes `resistance to beta-lactam` and
  `resistance to carbapenems`, matching the names bugphyzzExports writes
  (waldronlab/bugphyzzExports#83, @lwaldron).

## BUG FIXES AND MINOR IMPROVEMENTS

+ Cell length and width no longer collapse to the lower bound of their
  range (#275, @lwaldron).
+ Downloads are cached per version.
+ Levi Waldron is the maintainer (#257, @lwaldron).
+ Typos and missing terms in `attributes.tsv` are fixed (#281, @lwaldron).
+ `attributes.tsv` gains `sensitive to beta-lactam`, `sensitive to
  erythromycin A` and `sensitive to kanamycin A`, and accepts the name
  `butyrate-producing bacteria`, so the export passes `checkCuration()`
  (waldronlab/bugphyzzExports#99, @lwaldron).
+ Tests that download data skip when offline (#266, @lwaldron).

# bugphyzz 0.99.3
+ Update table with validations results in extdata to version 1.0.2

# bugphyzz 0.99.0
+ First version of the package for submission in Bioconductor.
