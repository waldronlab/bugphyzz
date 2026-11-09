## Tests of the ASR filtering in importBugphyzz() on small mocked inputs, so
## they run without downloading the exports.

mockExports <- function() {
    binary <- data.frame(
        NCBI_ID = 1:5, Taxon_name = paste0("t", 1:5), Rank = "species",
        Attribute = "spore formation", Attribute_value = TRUE,
        Evidence = c("exp", "asr", "asr", "asr", "exp"),
        Frequency = c("always", "usually", "rarely", "never", "rarely"),
        Score = c(1, 0.9, 0.2, 0, 0.1),
        Attribute_source = c("BacDive", NA, NA, NA, "BacDive"),
        Confidence_in_curation = c("high", NA, NA, NA, "high"),
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

mockValidationAttributes <- function() {
    data.frame(
        rank = "all",
        physiology = c(
            "spore formation", "aerophilicity", "COGEM pathogenicity rating",
            "genome size", "width"
        ),
        score_lcb = c(0.88, 0.79, 0.55, 0.80, 0.00),
        stringsAsFactors = FALSE
    )
}

importMocked <- function(..., env = parent.frame()) {
    testthat::local_mocked_bindings(
        .downloadResource = function(version, forceDownload, dropped = FALSE)
            mockExports(),
        .validationAttributes = function() mockValidationAttributes(),
        .env = env
    )
    importBugphyzz(...)
}

test_that("defaults keep whole attributes with a lower bound of at least 0.7 (0.5)", {
    bp <- importMocked()
    ## spore formation: imputed rows with Score >= 0.5 kept, curated "rarely" dropped
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:2)
    ## aerophilicity passes, so every imputed state is kept
    expect_equal(
        bp[["aerophilicity"]]$Attribute_value,
        c("aerobic", "anaerobic", "facultatively anaerobic")
    )
    expect_true(is.na(bp[["aerophilicity"]]$Validation[1]))
    expect_equal(bp[["aerophilicity"]]$Validation[2:3], c(0.79, 0.79))
    ## COGEM fails (0.55): only the curated row is left
    expect_equal(bp[["COGEM pathogenicity rating"]]$Evidence, "nas")
    ## numeric: genome size (0.80) kept, width (0.00) dropped
    expect_equal(nrow(bp[["genome size"]]), 2)
    expect_equal(bp[["width"]]$Evidence, "exp")
    expect_true("NSTI" %in% names(bp[["genome size"]]))
})

test_that("validation join is case-insensitive and multistate values are lower case", {
    bp <- importMocked(kappaThreshold = 0.5)
    cogem <- bp[["COGEM pathogenicity rating"]]
    expect_equal(nrow(cogem), 3)
    expect_equal(cogem$Validation, c(NA, 0.55, 0.55))
    expect_true(all(cogem$Attribute_value == tolower(cogem$Attribute_value)))
})

test_that("thresholds apply by attribute type", {
    bp <- importMocked(kappaThreshold = 0.9, r2Threshold = 0)
    expect_equal(bp[["aerophilicity"]]$Evidence, "exp")
    expect_equal(nrow(bp[["width"]]), 2)
    bp <- importMocked(kappaThreshold = 0, r2Threshold = 0.9)
    expect_equal(nrow(bp[["COGEM pathogenicity rating"]]), 3)
    expect_equal(bp[["genome size"]]$Evidence, "igc")
})

test_that("asrScoreThreshold filters imputed rows only", {
    bp <- importMocked(asrScoreThreshold = 0.95)
    expect_equal(bp[["spore formation"]]$Evidence, "exp")
    expect_equal(bp[["aerophilicity"]]$Evidence, "exp")
    bp <- importMocked(asrScoreThreshold = 0)
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:4)
})

test_that("excludeRarely is deprecated; FALSE means asrScoreThreshold = 0", {
    expect_warning(bp <- importMocked(excludeRarely = FALSE), "deprecated")
    ## imputed rarely/never kept, curated rarely still excluded
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:4)
    expect_warning(bp <- importMocked(excludeRarely = TRUE), "deprecated")
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:2)
})

test_that("v is deprecated and sets both thresholds", {
    expect_warning(bp <- importMocked(v = 0.85), "deprecated")
    ## 0.85 drops aerophilicity (0.79) and genome size (0.80), keeps spore formation (0.88)
    expect_equal(bp[["aerophilicity"]]$Evidence, "exp")
    expect_equal(bp[["genome size"]]$Evidence, "igc")
    expect_equal(bp[["spore formation"]]$NCBI_ID, 1:2)
})

test_that("bad thresholds error", {
    expect_error(importMocked(kappaThreshold = 2), "kappaThreshold")
    expect_error(importMocked(r2Threshold = NA_real_), "r2Threshold")
    expect_error(importMocked(asrScoreThreshold = "a"), "asrScoreThreshold")
})

test_that("only thresholds below the defaults ask for the dropped imputations", {
    asked <- NULL
    local_mocked_bindings(
        .downloadResource = function(version, forceDownload, dropped = FALSE) {
            asked <<- c(asked, dropped)
            mockExports()
        },
        .validationAttributes = function() mockValidationAttributes()
    )
    importBugphyzz()
    importBugphyzz(kappaThreshold = 0.8, asrScoreThreshold = 0.9)
    importBugphyzz(kappaThreshold = 0.6)
    importBugphyzz(r2Threshold = 0.4)
    importBugphyzz(asrScoreThreshold = 0.2)
    expect_equal(asked, c(FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("shipped validation table is v1.0.7 and has one row per key", {
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
    ## v1.0.7: predictions called positive at the shipped score (>= 0.5);
    ## shape "filamentous" passes at genus rank, anaerobic reaches 0.9
    fil <- v[v$rank == "genus" & v$attribute == "filamentous" & v$physiology == "shape", ]
    expect_equal(nrow(fil), 1L)
    expect_gte(fil$mcc_mean, 0.7)
    expect_gte(aer$mcc_mean, 0.68)
    expect_gte(all_rank$mcc_mean[all_rank$attribute == "anaerobic"], 0.9)
})


droppedFixture <- function() {
    cols <- function(...) data.frame(..., stringsAsFactors = FALSE)
    main <- list(
        binary = cols(NCBI_ID = "562", Taxon_name = "Escherichia coli", Rank = "species",
            Attribute = "motility", Attribute_value = "TRUE", Evidence = "exp",
            Frequency = "always", Score = 1, Attribute_source = "BacDive",
            Confidence_in_curation = "high", Attribute_type = "binary"),
        multistate = cols(NCBI_ID = "1279", Taxon_name = "Staphylococcus", Rank = "genus",
            Attribute = "COGEM pathogenicity rating", Attribute_value = "COGEM pathogenicity rating 1",
            Evidence = "nas", Frequency = "always", Score = 1,
            Attribute_source = "The Microbe Directory", Confidence_in_curation = "low",
            Attribute_type = "multistate-intersection"),
        numeric = cols(NCBI_ID = "562", Taxon_name = "Escherichia coli", Rank = "species",
            Attribute = "length", Attribute_value = 2, Evidence = "exp",
            Frequency = "always", Score = 1, Attribute_source = "BacDive",
            Confidence_in_curation = "high", Attribute_type = "numeric", nsti = NA_real_)
    )
    dropped <- cols(NCBI_ID = c("1280", "1281"), Taxon_name = c("a", "b"), Rank = "species",
        Attribute = "COGEM pathogenicity rating",
        Attribute_value = "COGEM pathogenicity rating 2",
        Evidence = "asr", Frequency = c("sometimes", "rarely"), Score = c(0.7, 0.2),
        Attribute_source = NA, Confidence_in_curation = NA,
        Attribute_type = "multistate-intersection", Validation = 0.55)
    tmp <- tempfile(fileext = ".csv.gz")
    con <- gzfile(tmp, "w"); cat("# header\n", file = con)
    suppressWarnings(write.table(dropped, con, sep = ",", row.names = FALSE,
        append = TRUE, col.names = TRUE))
    close(con)
    list(main = main, droppedFile = tmp)
}

test_that("lower thresholds bring back dropped imputations from GitHub versions", {
    fx <- droppedFixture()
    local_mocked_bindings(
        .urlExists = function(url) TRUE,
        .getResource = function(rname, url, ...) {
            if (grepl("asr_dropped", rname)) return(fx$droppedFile)
            f <- tempfile(fileext = ".csv")
            tbl <- fx$main[[sub("^bugphyzz_([a-z]+)_.*$", "\\1", rname)]]
            writeLines("# header", f)
            suppressWarnings(utils::write.table(tbl, f, sep = ",",
                row.names = FALSE, append = TRUE))
            f
        },
        .validationAttributes = function() mockValidationAttributes()
    )
    bp <- suppressMessages(importBugphyzz(version = "devel"))
    expect_equal(bp[["cogem pathogenicity rating"]]$Evidence, "nas")
    bp <- suppressMessages(importBugphyzz(version = "devel", kappaThreshold = 0.5))
    expect_equal(sum(bp[["cogem pathogenicity rating"]]$Evidence == "asr"), 1L)
    bp <- suppressMessages(importBugphyzz(
        version = "devel", kappaThreshold = 0.5, asrScoreThreshold = 0
    ))
    expect_equal(sum(bp[["cogem pathogenicity rating"]]$Evidence == "asr"), 2L)
    expect_type(bp[["cogem pathogenicity rating"]]$Score, "double")
})

test_that("a version without the dropped file warns and keeps what it ships", {
    fx <- droppedFixture()
    local_mocked_bindings(
        .urlExists = function(url) !grepl("asr_dropped", url),
        .getResource = function(rname, url, ...) {
            f <- tempfile(fileext = ".csv")
            tbl <- fx$main[[sub("^bugphyzz_([a-z]+)_.*$", "\\1", rname)]]
            writeLines("# header", f)
            suppressWarnings(utils::write.table(tbl, f, sep = ",",
                row.names = FALSE, append = TRUE))
            f
        }
    )
    expect_warning(
        out <- suppressMessages(.downloadGH("abc1234", FALSE, dropped = TRUE)),
        "no file of dropped"
    )
    expect_length(out, 3)
})

test_that("shipped attribute-level validation table has one row per rank and physiology", {
    v <- .validationAttributes()
    expect_true(all(c("rank", "physiology", "metric", "score_lcb") %in% names(v)))
    expect_false(anyDuplicated(v[c("rank", "physiology")]) > 0)
    expect_true(all(v$metric %in% c("kappa", "weighted kappa", "r2")))
    expect_true(all(v$score_lcb <= v$score_mean, na.rm = TRUE))
})
