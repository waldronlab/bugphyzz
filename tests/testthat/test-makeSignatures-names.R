## Offline checks of signature names: discrete and numeric signatures share
## the "bugphyzz:" prefix (#256).

mockDiscrete <- function() {
    data.frame(
        NCBI_ID = as.character(1:12), Taxon_name = paste0("t", 1:12),
        Rank = "species", Attribute = "aerophilicity",
        Attribute_value = "aerobic", Evidence = "exp", Frequency = "always",
        Attribute_type = "multistate-intersection",
        stringsAsFactors = FALSE
    )
}

mockNumeric <- function() {
    data.frame(
        NCBI_ID = as.character(1:12), Taxon_name = paste0("t", 1:12),
        Rank = "species", Attribute = "genome size",
        Attribute_value = seq(1e6, 2e6, length.out = 12), Evidence = "exp",
        Frequency = "always", Attribute_type = "range",
        stringsAsFactors = FALSE
    )
}

test_that("discrete signatures are named bugphyzz:<attribute>|<value>", {
    s <- makeSignatures(mockDiscrete())
    expect_identical(names(s), "bugphyzz:aerophilicity|aerobic")
    expect_length(s[[1]], 12)
})

test_that("numeric signatures use the same prefix", {
    s <- makeSignatures(mockNumeric(), min = 1e6, max = 2e6)
    expect_match(names(s), "^bugphyzz:genome size\\|")
    s <- makeSignatures(mockNumeric())
    expect_true(all(startsWith(names(s), "bugphyzz:")))
})
