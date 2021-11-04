
phys <- physiologies()
phys[["fatty acid composition"]] <- fattyAcidComposition()

# test_that("all datasets pass curation tests", {
#     expect_null(bugphyzz:::.checkColumnValuesList(phys))
#     expect_null(bugphyzz:::.checkRequiredColumnsList(phys))
# })

test_that("the NCBI_ID column is not of type numeric.", {
    ## If the imported NCBI_ID column is of class numeric, this could mean
    ## that some NCBI_ID values are too high and unlikely to be valid taxonomy
    ## ids in the NCBI database.
    err_tbl <- .checkColumnValuesList(phys)

    filter_tbl <-
        err_tbl[["error_type"]] == "Invalid class" &
        err_tbl[["col"]] == "NCBI_ID" &
        err_tbl[["invalid_class"]] == "numeric"

    ncbi_id_class_err <- err_tbl[filter_tbl,]

    expect_length(ncbi_id_class_err, 0)
})
