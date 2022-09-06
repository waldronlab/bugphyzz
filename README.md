# bugphyzz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bugphyzz)](https://CRAN.R-project.org/package=bugphyzz)
<!-- badges: end -->
 
Bugphyzz is a collection of physiological features about bacteria. We are curating the databases through 3 main methods: webscraping, bulk download from online databases, and manual curation from literature. Mainly this database will be used to analyze microbiome data. BugPhyzz can allow a user to look at specific physiologies, and make signature sets with specific bugs.


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

Some analyses using data provided by this package can be found at https://waldronlab.io/bugphyzzAnalyses/

## How to add a new attribute in GitHub to the BugPhyzz database
 1. Add attributes of physiology to Attributes using ontology terms
 2. Add reference(s) to Confidence in Curation
 3. Add Google Doc "publish to web" link and add in Links
 4. Add Google Doc link to Source Link
