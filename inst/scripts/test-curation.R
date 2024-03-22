
phys <- physiologies()
phys[["fatty acid composition"]] <- fattyAcidComposition()


# Functions ---------------------------------------------------------------

## A function for testing for validity of NCBI_IDs
check_valid_NCBI_IDs <- function(dat) {

  ncbi_ids <- dat[["NCBI_ID"]]
  taxonomies <- taxizedb::classification(ncbi_ids, db = "ncbi")
  invalid_positions_lgl <- !is.na(ncbi_ids) & is.na(taxonomies)
  names(invalid_positions_lgl) <- NULL
  invalid_positions <- which(invalid_positions_lgl)

  if (!length(invalid_positions)) {
    message("All NCBI IDs are valid.")
    return(NULL)
  }

  invalid_values <- ncbi_ids[invalid_positions]

  list(invalid_positions = invalid_positions, invalid_values = invalid_values)

  .stop_custom("invalid_ncbi_ids", message = "Invalid NCBI IDs.",
    invalid_positions = invalid_positions, invalid_values = invalid_values)
}


# Tests -------------------------------------------------------------------

test_that("the NCBI_ID column is not of type numeric.", {
#     ## If the imported NCBI_ID column is of class numeric, this could mean
#     ## that some NCBI_ID values are too high and unlikely to be valid taxonomy
#     ## ids in the NCBI database.
    err_tbl <- .checkColumnValuesList(phys)

    filter_tbl <-
        err_tbl[["error_type"]] == "Invalid class" &
        err_tbl[["col"]] == "NCBI_ID" &
        err_tbl[["invalid_class"]] == "numeric"

    ncbi_id_class_err <- err_tbl[filter_tbl,]

    expect_length(ncbi_id_class_err, 0)
})


test_that("all NCBI_IDs are valid.", {

  err_list <- lapply(phys, function(x) {
    tryCatch(
      invalid_ncbi_ids = function(e) e,
      check_valid_NCBI_IDs(x)
    )
  }) %>%
    discard(is.null)
  expect_length(err_list, 0)
})

test_that("all datasets pass curation tests", {
    expect_null(bugphyzz:::.checkColumnValuesList(phys))
    expect_null(bugphyzz:::.checkRequiredColumnsList(phys))
})
