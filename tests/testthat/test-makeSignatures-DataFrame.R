testthat::skip_if_offline()
library(purrr)
library(S4Vectors)
bp <- map(importBugphyzz(), DataFrame)
sigsNames <- map(bp, ~ makeSignatures(.x, taxIdType = "Taxon_name")) |>
    list_flatten(name_spec = "{inner}")
sigsIDs <- map(bp, ~ makeSignatures(.x, taxIdType = "NCBI_ID")) |>
    list_flatten(name_spec = "{inner}")

test_that("makeSignatures works with IDs", {
    expect_true(all(map_lgl(sigsIDs, is.integer)))
})
test_that("makeSignatures works with taxon names", {
    expect_true(all(map_lgl(sigsNames, is.character)))
})
