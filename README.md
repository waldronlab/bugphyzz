# bugphyzz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bugphyzz)](https://CRAN.R-project.org/package=bugphyzz)
<!-- badges: end -->

Bugphyzz is a collection of physiological features about bacteria. We are curating the databases through 3 main methods: webscraping, bulk download from online databases, and manual curation from literature. Mainly this database will be used to analyze microbiome data.

## Definitions
* **NCBI ID**: National Center for Bioinfomatic numeric ID to link to specific taxa.
* **Genome ID**: From NCBI, another type of numeric ID, that links to a specific sequenced genome. Not all taxa have a NCBI ID, and might only have a Genome ID
* **Accession ID**: The accession number is a unique identifier assigned to a record in sequence databases such as GenBank. Several NCBI databases use the format [alphabetical prefix][series of digits]. A change in the record in some databases (e.g. GenBank) is tracked by an integer extension of the accession number, an Accession.
* **Taxon name**: a human readable organismal name that can delinate which level taxa the annotation is describing.
* **Attribute**: a value for a physiology, that is mapped to an ontology whenever possible. These are defined in the `attribute` column of [inst/extdata/attributes.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv
 **Physiology**: a microbial characteristic that can be measured or observed, that should be mapped to an ontology wherever possible. These are defined in [inst/extdata/links.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/links.tsv).
* **Attribute value**: a value describing a physiology. Allowable values are defined by the `validity` column of [inst/extdata/attributes.tsv](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv). For categorical attributes (such as "obligately aerobic"), values can be "TRUE" and "FALSE", for example. For numeric attributes, these can be numeric values.
* **Attribute source**: a citeable domain which the annotation was obtained. A source can be another database or primary literature.
* **Frequency**: Describes how often the attritbute (phenotype) occurs: Rarely, Sometimes, Usually, Always, or Unknown. If the source makes the attribute unclear how often it occurs, then an unknown is given.
* **Evidence codes**: As the Gene Ontology defines different forms of evidence a source was obtained the annotation. 

EXP: Experimental data, the attribute was obtained through labortory confirmations, and ideally more than once.

IGC: Computational predicted through means of metagenomic analysis, or more broadly, genome analysis.

ASR: Anstrestal State Reconstruction, was predicted using ASR computional techniques to predicted up and down the taxonmic tree of taxa that has a value.

* **Note**: Any additional information that does not fit in any fixed column. The information here is variable.
* **MIDAS ID**: Located in the Habitat attribute. Any annotation from the MIDAS database is assigned an ID number. We provided that ID here.
* **Parent name**: The parent of descendant given in Taxon name column. Values can indudes names or NA.
* **Species name**: The species name, or descendant of parent, given in the Taxon name column. Values can indudes names or NA.
* **Unit**: defines unit of attribute, like µm.
* **Rank**: The taxonomic rank of the annotation. Values can indudes names or NA.
* **Parent Rank**: The taxonmic rank of the parent of the annotation. Values can indudes names or NA.

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
