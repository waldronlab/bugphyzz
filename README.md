# bugphyzz (bacterial physiologies)

bugphyzz is a collection of physiological features about bacteria to be used as a resource for physiological discovery and producing signatures. We are continuously curating bugphyzz databases through 3 main methods: webscraping, bulk download from online databases, and manual curation from literature. Mainly this database was concieve for analyzing microbiome data using physiological information. BugPhyzz can allow a user to look at specific physiologies, and make signature sets with specific bugs. The uniform structure of the data is what allows for the latter to manifest.

We curated informatiob like Gram stain, optimal temperature, fermenation, respiration, size, shape, colony arrangement, and 27 more physiologies. We have made BugPhyzz accessible via direct download or through our R package.

Here, you can find installation instructions, examples of making signatures, bugphyzz analysis link and how to add your own curation to the database.

## Installation

You can install the development version of bugphyzz with:


```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

## Devel version
BiocManager::install("waldronlab/bugphyzz")
```r

## Some examples of how to create signatures 

*** Need to update code for examples ***


## bugphyzzAnalyses

Some analyses using data provided by this package can be found at https://waldronlab.io/bugphyzzAnalyses/

## How to add a new attribute in GitHub to the bugphyzz database
Curation steps:
 1. Add attributes of physiology to [Attributes](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/attributes.tsv) using ontology terms
 2. Add reference(s) to [Confidence in Curation](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/confidence_in_curation.tsv)

If using Google Sheets, publish a csv file to web:

 4. Add Google Doc "publish to web" link and add in [Link](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/links.tsv)
 5. Add Google Doc link to [Source Link](https://github.com/waldronlab/bugphyzz/blob/main/inst/extdata/source_links.tsv)
