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

test_that("shipped validation table is v1.0.6 and has one row per key", {
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
    ## v1.0.6: no rows for physiologies that could not be validated, and
    ## COGEM rating 3 passes MCC 0.7
    expect_false(any(v$physiology %in% c("host-associated", "biofilm formation")))
    cog3 <- all_rank[all_rank$attribute == "COGEM pathogenicity rating 3", ]
    expect_equal(nrow(cog3), 1L)
    expect_gte(cog3$mcc_mean, 0.7)
})

test_that("includeDroppedAsr refuses a Zenodo version", {
    expect_error(
        .addDroppedAsr(list(), "10.5281/zenodo.12574596", FALSE),
        "GitHub version"
    )
})

test_that("includeDroppedAsr brings dropped ASR rows under a lower threshold", {
    cols <- function(...) data.frame(..., stringsAsFactors = FALSE)
    main <- list(
        binary = cols(NCBI_ID = "562", Taxon_name = "Escherichia coli", Rank = "species",
            Attribute = "motility", Attribute_value = "TRUE", Evidence = "exp",
            Frequency = "always", Score = 1, Attribute_source = "BacDive",
            Confidence_in_curation = "high", Attribute_type = "binary", NSTI = NA_real_),
        multistate = cols(NCBI_ID = "1279", Taxon_name = "Staphylococcus", Rank = "genus",
            Attribute = "aerophilicity", Attribute_value = "anaerobic", Evidence = "asr",
            Frequency = "usually", Score = 0.8, Attribute_source = NA_character_,
            Confidence_in_curation = NA_character_, Attribute_type = "multistate-intersection", NSTI = 0.1),
        numeric = cols(NCBI_ID = "562", Taxon_name = "Escherichia coli", Rank = "species",
            Attribute = "length", Attribute_value = "2", Evidence = "exp",
            Frequency = "always", Score = 1, Attribute_source = "BacDive",
            Confidence_in_curation = "high", Attribute_type = "numeric", nsti = NA_real_)
    )
    dropped <- cols(NCBI_ID = c("1279", "1279"), Taxon_name = "Staphylococcus", Rank = "genus",
        Attribute = "aerophilicity", Attribute_value = c("aerobic", "aerobic"),
        Evidence = "asr", Frequency = c("usually", "never"), Score = c(0.7, 0.1),
        Attribute_source = NA, Confidence_in_curation = NA,
        Attribute_type = "multistate-intersection", NSTI = 0.1, Validation = 0.66)
    tmp <- tempfile(fileext = ".csv.gz")
    con <- gzfile(tmp, "w"); cat("# header\n", file = con)
    suppressWarnings(write.table(dropped, con, sep = ",", row.names = FALSE, append = TRUE, col.names = TRUE))
    close(con)
    val <- data.frame(rank = "all", physiology = "aerophilicity",
        attribute = c("aerobic", "anaerobic"), mcc_mean = c(0.66, 0.85), r2_mean = NA_real_,
        value = c(0.66, 0.85))
    local_mocked_bindings(
        .downloadResource = function(version, forceDownload) main,
        .getResource = function(...) tmp,
        .validationData = function() val
    )
    bp <- importBugphyzz(version = "devel", includeDroppedAsr = TRUE)
    expect_equal(sort(unique(bp$aerophilicity$Attribute_value)), "anaerobic")
    bp <- importBugphyzz(version = "devel", includeDroppedAsr = TRUE, mccThreshold = 0.6)
    expect_equal(sum(bp$aerophilicity$Attribute_value == "aerobic"), 1L)   # "never" row still excluded
    expect_false("Validation" %in% names(bp$aerophilicity) && all(is.na(bp$aerophilicity$Validation)))
    bp <- importBugphyzz(version = "devel", includeDroppedAsr = TRUE, mccThreshold = 0.6, excludeRarely = FALSE)
    expect_equal(sum(bp$aerophilicity$Attribute_value == "aerobic"), 2L)
    expect_type(bp$aerophilicity$Score, "double")
    bp <- importBugphyzz(version = "devel", includeDroppedAsr = FALSE)
    expect_false("aerobic" %in% bp$aerophilicity$Attribute_value)
})
