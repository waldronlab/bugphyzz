testthat::skip_if_offline()
library(purrr)
pnames <- showPhys()
p <- suppressWarnings(physiologies())
test_that("physiologies works", {
    expect_true(all(map_lgl(p, is.data.frame)))
    expect_identical(names(p), pnames[pnames %in% names(p)])
    ## a physiology may be missing only if it has no sheet: it is read from
    ## bugphyzzExports sources/, which can lag behind this package
    links <- utils::read.delim(
        system.file("extdata", "spreadsheet_links.tsv", package = "bugphyzz")
    )
    sourceOnly <- links$physiology[is.na(links$link) | !nzchar(links$link)]
    expect_true(all(setdiff(pnames, names(p)) %in% sourceOnly))
})

## Every discrete value from every sheet and source must be in the controlled
## vocabulary (attributes.tsv) after the synonym table is applied; the export
## drops anything else. Free-text groups the export does not use are skipped.
test_that("every discrete attribute value is in the vocabulary", {
    freeText <- c(
        "growth medium", "isolation site", "metabolite production",
        "metabolite utilization", "country", "geographic location"
    )
    discrete <- p[map_lgl(p, ~ all(unique(.x$Attribute_type) %in% c(
        "binary", "multistate-intersection", "multistate-union"
    )))]
    discrete <- discrete[!names(discrete) %in% freeText]
    invalid <- imap(discrete, function(df, g) {
        setdiff(unique(df$Attribute), bugphyzz:::.validAttributes(g))
    })
    invalid <- invalid[lengths(invalid) > 0]
    expect_identical(
        invalid, setNames(list(), character(0)),
        label = "unmapped values (add them to attribute_synonyms.tsv)"
    )
})
