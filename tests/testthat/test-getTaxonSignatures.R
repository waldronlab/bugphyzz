testthat::skip_if_offline()
taxID <- "562"
taxName <- "Escherichia coli"
bp <- importBugphyzz()
sigs_ids <- getTaxonSignatures(taxID, bp)
sigs_tax <- getTaxonSignatures(
    tax = taxName, bp = bp, taxIdType = "Taxon_name"
)
test_that("getTaxonSignatures works with IDs", {
    expect_gt(length(sigs_ids), 0)
    expect_type(sigs_ids, "character")
})
test_that("getTaxonSignatures works with IDs", {
    expect_gt(length(sigs_tax), 0)
    expect_type(sigs_tax, "character")
})
