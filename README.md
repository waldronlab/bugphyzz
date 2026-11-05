# bugphyzz

[![Bioconductor CI](https://github.com/waldronlab/bugphyzz/actions/workflows/ci.yml/badge.svg)](https://github.com/waldronlab/bugphyzz/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/waldronlab/bugphyzz/graph/badge.svg)](https://codecov.io/gh/waldronlab/bugphyzz)

`bugphyzz` is a harmonized database of bacterial and archaeal physiological traits (aerophilicity, Gram stain, shape, growth temperature, G+C content, habitat, and about 30 more), collected from curated and programmatic sources, extended to uncharacterized taxa by ancestral state reconstruction, and delivered as an R/Bioconductor package for building microbial signatures for bug set enrichment analysis. Site: https://waldronlab.io/bugphyzz/

## Installation

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("bugphyzz")            # release
BiocManager::install("waldronlab/bugphyzz") # devel from GitHub
```

## Usage

```r
library(bugphyzz)
bp <- importBugphyzz()                       # list of data.frames, one per attribute
sigs <- makeSignatures(bp[["aerophilicity"]], taxIdType = "NCBI_ID", taxLevel = "genus")
getTaxonSignatures("562", bp)                # the 21 signatures containing E. coli
```

`importBugphyzz()` downloads the released annotations from Zenodo (the release pinned in its `version` default; pass another DOI, or `version = "devel"` for the weekly export) and keeps imputed annotations only where they passed cross-validation (`mccThreshold`, `r2Threshold`). See `vignette("bugphyzz")` for the data schema and an enrichment example, and `vignette("sources")` for the sources.

## How the data are produced

The package is the user-facing end of a pipeline spread over several repositories.

| Step | Where | What |
|---|---|---|
| 1. Curation | Google Sheets listed in [`inst/extdata/spreadsheet_links.tsv`](inst/extdata/spreadsheet_links.tsv) | Hand-curated annotations, one sheet per physiology, with the controlled vocabulary in [`inst/extdata/attributes.tsv`](inst/extdata/attributes.tsv) and the sources in [`inst/extdata/attribute_sources.tsv`](inst/extdata/attribute_sources.tsv). |
| 2. Programmatic sources | [bugphyzzExports](https://github.com/waldronlab/bugphyzzExports), `sources/` | Scripted imports kept under version control with the script and pinned upstream version: Bergey's Manual genus abstracts (LLM extraction), Madin et al. 2020, BacDive (API). Corrections go in each source's `overrides.tsv`, never in the output. |
| 3. Raw import | this package, `physiologies()` | Reads steps 1 and 2 into one tidy table per physiology; for curators and the export, not for users. |
| 4. Harmonization, conflict resolution, propagation | [taxPPro](https://github.com/waldronlab/taxPPro) (R package) and [bugphyzzExports](https://github.com/waldronlab/bugphyzzExports) `inst/scripts/export_bugphyzz.R` | Resolves conflicting annotations by source confidence, propagates strain annotations to species, runs ancestral state reconstruction on the LTP tree (phytools for discrete, castor for numeric), applies the validation thresholds, writes CSV and GMT files. A GitHub Action runs it weekly and commits to `devel`. |
| 5. Cross-validation | [taxPProValidation](https://github.com/waldronlab/taxPProValidation) | 10-fold cross-validation of the reconstruction per attribute value; its `validation_summary.tsv` is copied to [`inst/extdata/`](inst/extdata/) here and decides which imputed annotations ship. Re-run when sources change. |
| 6. Release | [Zenodo](https://zenodo.org/doi/10.5281/zenodo.10980653) | Versioned deposits of the exported files; `importBugphyzz()` pins one release as its default and is updated with each deposit. |
| 7. Analyses and paper | [bugphyzzAnalyses](https://github.com/waldronlab/bugphyzzAnalyses), [bugphyzz-manuscript](https://github.com/waldronlab/bugphyzz-manuscript) (private) | Articles reproducing the manuscript figures; manuscript issues. |

Other related repositories: [taxoniq](https://github.com/lwaldron/taxoniq) (dated NCBI taxonomy releases for taxid lookups), [BugSigDB](https://bugsigdb.org) and [bugsigdbr](https://bioconductor.org/packages/bugsigdbr) (published signatures used in the enrichment analyses), [MicrobiomeBenchmarkData](https://github.com/waldronlab/MicrobiomeBenchmarkData) (benchmark datasets).

## Contributing

- **Data corrections**: open an issue here with the `curation` label, naming the taxon, attribute, source and the sheet or `sources/` file; or edit the sheet directly if you have access. Corrections to programmatic sources go in that source's `overrides.tsv` in bugphyzzExports.
- **New sources**: add a directory under `sources/` in bugphyzzExports following its README (script, pinned version, README, manifest row), then a row in `attribute_sources.tsv` here.
- **New attributes**: add the values to `attributes.tsv`, the physiology to `spreadsheet_links.tsv` (a sheet link, or none for a source-only attribute), signature bins to `thresholds.tsv` for numeric attributes, and the attribute to the list in `export_bugphyzz.R`.
- **Code**: pull requests get an automatic review; `R CMD check` and `BiocCheck` run in CI. Keep `NEWS.md` current with user-facing changes.
