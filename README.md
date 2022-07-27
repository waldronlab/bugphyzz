# bugphyzz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bugphyzz)](https://CRAN.R-project.org/package=bugphyzz)
<!-- badges: end -->

Bugphyzz is a collection of physiological features about bacteria. We are curating the databases through 3 main methods: webscraping, bulk download from online databases, and manual curation from literature. Mainly this database will be used to analyze microbiome data.

## Definitions

* **physiology**: a microbial characteristic that can be measured or observed, that should be mapped to an ontology wherever possible. These are defined in [inst/extdata/links.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/links.tsv)
* **attribute**: a possible value for a physiology, that should be mapped to an ontology wherever possible. These are defined in the `attribute` column of [inst/extdata/attributes.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv)
* **attribute value**: a possible value for an attribute. Allowable values are defined by the `validity` column of [inst/extdata/attributes.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv). For categorical attributes (such as "obligately aerobic"), values can be "TRUE" and "FALSE", for example. For numeric attributes, these can be numeric values.

## Documentation
**Evidence codes**

EXP: Experimental data, the attribute was obtained through labortory confirmations, and ideally more than once.
IGC: Computational predicted through means of metagenomic analysis, or more broadly, genome analysis.
ASR: Anstrestal State Reconstruction, was predicted using ASR computional techniques to predicted up and down the taxonmic tree of taxa that has a value.

## Installation

You can install the development version of bugphyzz with:

``` r
# install.packages("devtools")
devtools::install_github("waldronlab/bugphyzz")
```

## bugphyzzAnalyses

Some analyses using data provided by this package can be found at https://waldronlab.io/bugphyzzAnalyses/