# bugphyzz

`bugphyzz` is a collection of bacterial physiological features to be used as a
resource for physiological discovery and producing signatures.
We are continuously curating `bugphyzz` databases through 3 main methods:
webscraping, bulk download from online databases, and manual curation from
literature. Mainly this database was concieved for analyzing microbiome data
using physiological information. `Bugphyzz` can allow a user to look at
specific physiologies, and create signature sets with specific bugs. The
uniform structure of the data is what allows for the latter to manifest.

## Installation

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

##Release version
BiocManager::install("bugphyzz")

## Devel version
BiocManager::install("waldronlab/bugphyzz")
```

After installation, check vignettes with:

```r
browseVigenttes("bugphyzz")
```
