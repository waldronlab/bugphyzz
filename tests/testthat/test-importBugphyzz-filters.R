## Tests of the ASR filtering in importBugphyzz() on small mocked inputs, so
## they run without downloading the exports.

mockExports <- function() {
    binary <- data.frame(
        NCBI_ID = 1:4, Taxon_name = paste0("t", 1:4), Rank = "species",
        Attribute = "spore formation", Attribute_value = TRUE,
        Evidence = c("exp", "asr", "asr", "asr"),
        Frequency = c("always", "usually", "rarely", "never"),
        Score = c(1, 0.9, 0.2, 0), Attribute_source = c("BacDive", NA, NA, NA),
        Confidence_in_curation = c("high", NA, NA, NA),
        Attribute_type = "binary", stringsAsFactors = FALSE
    )
    multistate <- data.frame(
        NCBI_ID = 1:6, Taxon_name = paste0("t", 1:6), Rank = "species",
        Attribute = c(rep("aerophilicity", 3), rep("COGEM pathogenicity rating", 3)),
        Attribute_value = c(
            "aerobic", "anaerobic", "facultatively anaerobic",
            "COGEM pathogenicity rating 1", "COGEM pathogenicity rating 1",
            "COGEM pathogenicity rating 2"
        ),
        Evidence = c("exp", "asr", "asr", "nas", "asr", "asr"),
        Frequency = c("always", "usually", "usually", "always", "usually", "sometimes"),
        Score = c(1, 0.9, 0.9, 1, 0.9, 0.6), Attribute_source = NA,
        Confidence_in_curation = NA,
        Attribute_type = "multistate-intersection", stringsAsFactors = FALSE
    )
    numeric <- data.frame(
        NCBI_ID = 1:4, Taxon_name = paste0("t", 1:4), Rank = "species",
        Attribute = c("genome size", "genome size", "width", "width"),
        Attribute_value = c(3e6, 4e6, 0.5, 0.6),
        Evidence = c("igc", "asr", "exp", "asr"),
        Frequency = c("always", "usually", "always", "usually"),
        Score = c(1, 0.9, 1, 0.9), Attribute_source = NA,
        Confidence_in_curation = NA, Attribute_type = "numeric",
        nsti = c(NA, 0.05, NA, 0.05), stringsAsFactors = FALSE
    )
    list(binary = binary, multistate = multistate, numeric = numeric)
}

mockValidation <- function() {
    data.frame(
        rank = "all",
        physiology = c(
            "spore formation", "aerophilicity", "aerophilicity",
            "aerophilicity", "COGEM pathogenicity rating",
            "COGEM pathogenicity rating", "genome size", "width"
        ),
        attribute = c(
            "spore formation", "aerobic", "anaerobic",
            "facultatively anaerobic", "COGEM pathogenicity rating 1",
            "COGEM pathogenicity rating 2", "genome size", "width"
        ),
        mcc_mean = c(0.88, 0.71, 0.84, 0.23, 0.74, 0.71, NA, NA),
        r2_mean = c(NA, NA, NA, NA, NA, NA, 0.72, 0.04),
        stringsAsFactors = FALSE
    ) |>
        dplyr::mutate(value = ifelse(is.na(mcc_mean), r2_mean, mcc_mean))
}

importMocked <- function(...) {
    testthat::local_mocked_bindings(
        .downloadResource = function(version, forceDownload) mockExports(),
        .validationData = function() mockValidation()
    )
    importBugphyzz(...)
}

test_that("default thresholds keep MCC >= 0.7 and R2 >= 0.5 ASR rows", {
    bp <- importMocked()
    ## spore formation: ASR usually kept; rarely and never dropped
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:2)
    ## aerophilicity: anaerobic (0.84) kept, facultatively anaerobic (0.23) dropped
    expect_equal(bp[["aerophilicity"]]$Attribute_value, c("aerobic", "anaerobic"))
    ## curated rows never carry a validation score
    expect_true(is.na(bp[["aerophilicity"]]$Validation[1]))
    expect_equal(bp[["aerophilicity"]]$Validation[2], 0.84)
    ## numeric: genome size (0.72) kept, width (0.04) dropped
    expect_equal(nrow(bp[["genome size"]]), 2)
    expect_equal(nrow(bp[["width"]]), 1)
    expect_equal(bp[["width"]]$Evidence, "exp")
    expect_true("NSTI" %in% names(bp[["genome size"]]))
})

test_that("validation join is case-insensitive", {
    bp <- importMocked()
    expect_equal(nrow(bp[["COGEM pathogenicity rating"]]), 3)
    expect_equal(
        bp[["COGEM pathogenicity rating"]]$Validation, c(NA, 0.74, 0.71)
    )
})

test_that("thresholds apply by attribute type", {
    bp <- importMocked(mccThreshold = 0.9, r2Threshold = 0)
    expect_equal(bp[["aerophilicity"]]$Evidence, "exp")
    expect_equal(nrow(bp[["width"]]), 2)
    bp <- importMocked(mccThreshold = 0, r2Threshold = 0.9)
    expect_equal(nrow(bp[["aerophilicity"]]), 3)
    expect_equal(bp[["genome size"]]$Evidence, "igc")
})

test_that("excludeRarely = FALSE keeps rarely and never", {
    bp <- importMocked(excludeRarely = FALSE)
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:4)
})

test_that("v is deprecated and sets both thresholds", {
    expect_warning(bp <- importMocked(v = 0.8), "deprecated")
    ## 0.8 drops aerobic (0.71), COGEM (0.74, 0.71) and genome size (0.72)
    expect_equal(bp[["aerophilicity"]]$Attribute_value, c("aerobic", "anaerobic"))
    expect_equal(bp[["COGEM pathogenicity rating"]]$Evidence, "nas")
    expect_equal(bp[["genome size"]]$Evidence, "igc")
})

test_that("bad thresholds error", {
    expect_error(importMocked(mccThreshold = 2), "mccThreshold")
    expect_error(importMocked(r2Threshold = NA_real_), "r2Threshold")
    expect_error(importMocked(mccThreshold = "a"), "mccThreshold")
})

test_that("shipped validation table is v1.0.5 and has one row per key", {
    f <- system.file("extdata", "validation_summary.tsv", package = "bugphyzz")
    v <- utils::read.delim(f)
    expect_false(anyDuplicated(v[c("rank", "physiology", "attribute")]) > 0)
    all_rank <- v[v$rank == "all", ]
    gc <- all_rank[all_rank$physiology == "gc content", ]
    expect_equal(nrow(gc), 1L)
    expect_gte(gc$r2_mean, 0.5)
    aer <- all_rank[all_rank$attribute == "aerobic", ]
    expect_equal(nrow(aer), 1L)
    expect_false("tail" %in% all_rank$attribute[all_rank$physiology == "shape"])
})
