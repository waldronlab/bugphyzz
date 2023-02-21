# `bugphyzz` | bacterial physiologies

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bugphyzz)](https://CRAN.R-project.org/package=bugphyzz)
<!-- badges: end -->
 
`bugphyzz` is a collection of bacterial physiological features to be used as a resource for physiological discovery and producing signatures. We are continuously curating `bugphyzz` databases through 3 main methods: webscraping, bulk download from online databases, and manual curation from literature. Mainly this database was concieved for analyzing microbiome data using physiological information. `Bugphyzz` can allow a user to look at specific physiologies, and create signature sets with specific bugs. The uniform structure of the data is what allows for the latter to manifest.

We curated attributes like Gram stain, optimal temperature, fermentation, respiration, size, shape, colony arrangement, and 27 more physiologies. `bugphyzz` is now accessible via direct download or through our [R package](https://github.com/waldronlab/bugphyzz/edit/main/README.md#installation).

Here, you can find installation instructions, examples of creting signatures, `bugphyzz` [analyses](https://github.com/waldronlab/bugphyzz/edit/main/README.md#bugphyzzanalyses) and how to add your own curation to the database.


## Installation

You can install the development version of bugphyzz with:

``` r
# install.packages("devtools")
devtools::install_github("waldronlab/bugphyzz")
```

## Some examples of how to create signatures 

``` r
suppressMessages({
    library(bugphyzz)
    library(purrr)
    aer <- physiologies('aerophilicity')[[1]]
    ph <- physiologies('optimal ph')[[1]]
})

## Example 1
## Create a signature of genera with experimental evidence at a 
## Frequency of always using the NCBI taxids. Minimumn of 10 microbes per
## signature
aer_sig <- makeSignatures(
    aer, tax.id.type = 'NCBI_ID', tax.level = 'genus', 
    Frequency = 'always', Evidence = 'exp', min.size = 10
)
#> Warning: Dropping 3648 rows with FALSE values.
#> Creating signature for categorical attributes.

map_int(aer_sig, length)
#>                 aerobic               anaerobic facultatively anaerobic 
#>                     574                     294                     174 
#>         microaerophilic      obligately aerobic    obligately anaerobic 
#>                      17                      52                     104
map(aer_sig, ~ head(.x, 2))
#> $aerobic
#> [1]  291967 1078830
#> 
#> $anaerobic
#> [1] 1427378   33951
#> 
#> $`facultatively anaerobic`
#> [1]  2147 12914
#> 
#> $microaerophilic
#> [1] 46352 12960
#> 
#> $`obligately aerobic`
#> [1] 442869    434
#> 
#> $`obligately anaerobic`
#> [1] 258514  31980


## Example 2
## Create a signature of acidiphilic species using the Taxon names
acid_sig <- makeSignatures(
    df = ph, tax.id.type = 'Taxon_name', tax.level = 'species',
    max = 4
)
#> Creating signature for continuous attribute.
head(acid_sig[[1]])
#> [1] "Leptospirillum ferriphilum"     "Sulfobacillus acidophilus"     
#> [3] "Acidithiobacillus ferrivorans"  "Acidithiobacillus ferrooxidans"
#> [5] "Acidimicrobium ferrooxidans"    "Thermoplasma volcanium"

## Create a signature of Alkaliphilic species using the Taxon names
basic_sig <- makeSignatures(
    df = ph, tax.id.type = 'Taxon_name', tax.level = 'species', 
    min = 8
)
#> Creating signature for continuous attribute.
head(basic_sig[[1]])
#> [1] "Arhodomonas aquaeolei"        "Glaciecola nitratireducens"  
#> [3] "Pseudomonas coronafaciens"    "Sodalis glossinidius"        
#> [5] "Photorhabdus luminescens"     "Hyphomicrobium nitrativorans"
```

<sup>Created on 2022-09-01 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>

## bugphyzzAnalyses

Example analyses using data from this package can be found at https://waldronlab.io/bugphyzzAnalyses/

## How to add a new attribute in GitHub to the bugphyzz database
Curation steps:
 1. Add attributes of physiology to [Attributes](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv) using ontology terms
 2. Add reference(s) to [Confidence in Curation](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/confidence_in_curation.tsv)

If using Google Sheets, publish a csv file to web:

 4. Add Google Doc "publish to web" link and add in [Link](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/links.tsv)
 5. Add Google Doc link to [Source Link](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/source_links.tsv)
