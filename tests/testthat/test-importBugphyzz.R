testthat::skip_if_offline()

# Setup -------------------------------------------------------------------

library(purrr)

expected_columns_multistate <- c(
    NCBI_ID = "integer", Taxon_name = "character",
    Rank = "character",
    Attribute = "character", Attribute_value = "character",
    Evidence = "character",
    Frequency = "character", Score = "double", Attribute_source = "character",
    Confidence_in_curation = "character", Attribute_type = "character",
    Validation = "double"
)
expected_columns_binary <- c(
    NCBI_ID = "integer", Taxon_name = "character",
    Rank = "character",
    Attribute = "character", Attribute_value = "logical",
    Evidence = "character",
    Frequency = "character", Score = "double", Attribute_source = "character",
    Confidence_in_curation = "character", Attribute_type = "character",
    Validation = "double"
)
expected_columns_numeric <- c(
    NCBI_ID = "integer", Taxon_name = "character",
    Rank = "character",
    Attribute = "character", Attribute_value = "double",
    Evidence = "character",
    Frequency = "character", Score = "double", Attribute_source = "character",
    Confidence_in_curation = "character", Attribute_type = "character",
    NSTI = "double", Validation = "double"
)

checkColumnNames <- function(x) {
    attr_type <- unique(x$Attribute_type)
    if (attr_type == "multistate-intersection" | attr_type == "multistate-union") {
        lgl_vct <- colnames(x) == names(expected_columns_multistate)
    } else if (attr_type == "binary") {
        lgl_vct <- colnames(x) == names(expected_columns_binary)
    } else if (attr_type == "numeric") {
        lgl_vct <- colnames(x) == names(expected_columns_numeric)
    }
    return(all(lgl_vct))
}

checkColumnTypes <- function(x) {
    attr_type <- unique(x$Attribute_type)
    if ("Validation" %in% colnames(x)) {
        x$Validation <- as.double(x$Validation)
    }
    if ("NSTI" %in% colnames(x)) {
        x$NSTI <- as.double(x$NSTI)
    }
    types <- map_chr(x, typeof)
    if (attr_type == "multistate-intersection" | attr_type == "multistate-union") {
        lgl_vct <- types == expected_columns_multistate
    } else if (attr_type == "binary") {
        lgl_vct <- types == expected_columns_binary
    } else if (attr_type == "numeric") {
        lgl_vct <- types == expected_columns_numeric
    }
    return(all(lgl_vct))
}

checkNAs <- function(x) {
    select_col <- c(
        "NCBI_ID",
        "Taxon_name",
        "Rank",
        "Attribute",
        "Attribute_value",
        "Attribute_type",
        "Evidence",
        "Frequency",
        "Score"
    )
    x <- x[, select_col]
    !any(purrr::map_lgl(x, ~ any(is.na(.x))))
}

checkCuration <- function(x) {

    ## Valid values
    Rank_vals <- c(
        "superkingdom", "kingdom", "domain", "phylum", "class", "order",
        "family", "genus","species", "strain"
    )
    Attribute_type_vals <- c(
        "multistate-union", "multistate-intersection", "binary",
        "numeric"
    )
    Frequency_vals <- c(
        "always", "usually", "sometimes", "rarely", "never", "unknown"
    )
    Evidence_vals <- c(
        "exp", "igc", "nas", "tas", "tax", "asr"
    )
    Confidence_in_curation_vals <- c(
        "high", "medium", "low"
    )

    attr_type <- unique(x$Attribute_type)

    ## Columns omitted here are tested elsewhere
    ## Attribute_type for numeric values
    ## Score
    ## NSTI
    ## Validation
    ## NCBI_ID
    ## Taxon_name

    Rank_ok <- all(x$Rank %in% Rank_vals)
    Attribute_type_ok <- attr_type %in% Attribute_type_vals
    Evidence_ok <- all(x$Evidence %in% Evidence_vals)
    Frequency_ok <- all(unique(x$Frequency) %in% Frequency_vals)
    # Score_ok <- all(as.double(na.omit(x$Score)) >=0 & as.double(na.omit(x$Score <=1)))
    Confidence_in_curation_ok <- all(na.omit(x$Confidence_in_curation) %in% Confidence_in_curation_vals)

    ## Attribute_source
    srcs_tsv <- system.file("extdata", "attribute_sources.tsv", package = "bugphyzz")
    srcs <- readr::read_tsv(srcs_tsv, show_col_types = FALSE)
    Attribute_source_ok <- all(na.omit(x$Attribute_source) %in% srcs$Attribute_source)

    ## Attribute
    attrs_tsv <- system.file("extdata", "attributes.tsv", package = "bugphyzz")
    attrs_tbl <- readr::read_tsv(attrs_tsv, show_col_types = FALSE) |>
        dplyr::mutate(attribute_group = tolower(attribute_group))
    attrs <- attrs_tbl |>
        dplyr::mutate(attribute_group = strsplit(attribute_group, ";")) |>
        dplyr::pull(attribute_group) |>
        unlist() |>
        unique() |>
        {\(y)  y[!is.na(y)]}()
    Attribute_ok <- unique(x$Attribute) %in% attrs

    results <- c(
        Rank = Rank_ok,
        Attribute_type = Attribute_type_ok,
        Frequency = Frequency_ok,
        Evidence = Evidence_ok,
        Confidence_in_curation = Confidence_in_curation_ok,
        Attribute_source = Attribute_source_ok,
        Attribute = Attribute_ok
    )

    ## Attribute_value
    if (attr_type %in% c("multistate-union", "multistate-intersection")) {
        Attribute_value_vals <- attrs_tbl |>
            dplyr::filter(grepl(unique(x$Attribute), attribute_group)) |>
            dplyr::pull(attribute) |>
            unique() |>
            {\(y)  y[!is.na(y)]}() |>
            tolower()

        Attribute_value_ok <- all(x$Attribute_value %in% Attribute_value_vals)
        results <- c(results, Attribute_value = Attribute_value_ok)
    }
    return(all(results))
}

## Make sure that there are not duplicates by accident
checkUniqueAnnotations <- function(x) {
    dat <- x |>
        dplyr::select(NCBI_ID, Attribute, Attribute_value)
    !any(duplicated(dat))
}

# tests -------------------------------------------------------------------

test_that("importBugphyzz works with devel", {
    bp <- importBugphyzz(version = "devel", forceDownload = TRUE)
    expect_true(all("data.frame" == map_chr(bp, class)))
    expect_true(all(map_lgl(bp, ~ nrow(.x) > 0)))
    expect_true(all(map_lgl(bp, checkColumnNames)))
    expect_true(all(map_lgl(bp, checkColumnTypes)))
    expect_true(all(map_lgl(bp, checkNAs)))
    expect_true(all(map_lgl(bp, checkCuration)))
    expect_true(all(map_lgl(bp, checkUniqueAnnotations)))
})

test_that("importBugphyzz works with hash", {
    bp <- importBugphyzz(version = "8a09b46", forceDownload = TRUE)
    expect_true(all("data.frame" == map_chr(bp, class)))
    expect_true(all(map_lgl(bp, ~ nrow(.x) > 0)))
    expect_true(all(map_lgl(bp, checkColumnNames)))
    expect_true(all(map_lgl(bp, checkColumnTypes)))
    expect_true(all(map_lgl(bp, checkNAs)))
    expect_true(all(map_lgl(bp, checkCuration)))
    expect_true(all(map_lgl(bp, checkUniqueAnnotations)))
})

test_that("importBugphyzz works with Zenodo DOI", {
    bp <- importBugphyzz(version = "10.5281/zenodo.24166279", forceDownload = TRUE)
    expect_true(all("data.frame" == map_chr(bp, class)))
    expect_true(all(map_lgl(bp, ~ nrow(.x) > 0)))
    expect_true(all(map_lgl(bp, checkColumnNames)))
    expect_true(all(map_lgl(bp, checkColumnTypes)))
    expect_true(all(map_lgl(bp, checkNAs)))
    expect_true(all(map_lgl(bp, checkCuration)))
    expect_true(all(map_lgl(bp, checkUniqueAnnotations)))
})

## TODO create test for using Zenodo
test_that("importBugphyzz doesn't work with other words", {
    expect_error(importBugphyzz(version = "abcd-1234", forceDownload = TRUE))
})
