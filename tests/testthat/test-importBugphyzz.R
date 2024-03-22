
# Setup -------------------------------------------------------------------

library(purrr)
bp <- importBugphyzz()
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

# Tests -------------------------------------------------------------------

test_that("importBugphyzz works", {
  chr_vct <- map_chr(bp, class)
  expect_true(all("data.frame" == chr_vct))
})

test_that("All variable names are correct", {
  expect_true(all(map_lgl(bp, checkColumnNames)))
})

test_that("All variable types are correct", {
  expect_true(all(map_lgl(bp, checkColumnTypes)))
})
