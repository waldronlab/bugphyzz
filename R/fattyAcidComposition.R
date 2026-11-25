## Function for importing fatty acid compositions. The sheet is snapshotted as
## CSV in waldronlab/bugphyzzExports, sources/sheets/ (ADR 0007); the `link`
## column of spreadsheet_customlinks.tsv holds the path there, resolved against
## .sourcesBaseUrl() and read through the cache like .importSpreadsheets().
.fattyAcidComposition <- function(forceDownload = FALSE){
    relPath <- .customLinks() |>
        dplyr::filter(functionname == "fattyAcidComposition") |>
        dplyr::pull(link)
    fpath <- .getSourceResource(relPath, force = forceDownload)
    fac_wide <- utils::read.csv(fpath, check.names = FALSE)
    fac_long <- fac_wide |>
        tidyr::pivot_longer(
            cols = `Br-C10:1`:`Oxo-C19:1`,
            names_to = "Attribute_new", values_to = "Attribute_value"
        ) |>
        dplyr::mutate(NCBI_ID = as.character(NCBI_ID))
    dplyr::left_join(fac_long, ranksParents, by = "NCBI_ID") |>
        as.data.frame() |>
        .addSourceInfo() |>
        purrr::modify_at(
            .at = c(
                'Attribute', 'Frequency', 'Evidence', 'Confidence_in_curation'
            ),
            .f = ~ stringr::str_to_lower(.x)
        ) |>
        dplyr::select(-Attribute) |>
        dplyr::rename(Attribute = Attribute_new) |>
        .reorderColumns(attrType = 'numeric')
}

## Function to import custom links
.customLinks <- function(keyword = "all"){
    fname <-
        system.file("extdata/spreadsheet_customlinks.tsv", package = "bugphyzz")
    links <- utils::read.table(fname, sep = '\t', header = TRUE)
    ifelse(
        keyword[1] == "all", links,
        links <- links[links$physiology %in% keyword,]
    )
    links
}
