
test_that("the checkRequiredColumns function works.", {

  dataset_with_missing_required_columns <- data.frame(
    NCBI_ID = 1:10,
    Attribute = letters[1:10]
  )

  dataset_with_misplaced_required_columns <- data.frame(
    NCBI_ID = 1:10,
    Taxon_name = letters[1:10],
    Genome_ID = 1:10,
    Accession_number = 1:10,
    Attribute = 1:10,
    Attribute_value = 1:10,
    Attribute_source = 1:10,
    Evidence = 1:10,
    Note = 1:10,
    Confidence_interval = 1:10
  )

  dataset_with_required_columns_ok <- data.frame(
    NCBI_ID = 1:10,
    Genome_ID = 1:10,
    Accession_number = 1:10,
    Taxon_name = letters[1:10],
    Attribute = 1:10,
    Attribute_value = 1:10,
    Attribute_source = 1:10,
    Evidence = 1:10,
    Confidence_interval = 1:10,
    Note = 1:10
  )

  err1 <- checkRequiredColumns(dataset_with_missing_required_columns)
  err2 <- checkRequiredColumns(dataset_with_misplaced_required_columns)

  expect_s3_class(err1, "required_columns_missing")
  expect_s3_class(err2, "required_columns_misplaced")
  expect_message(checkRequiredColumns(dataset_with_required_columns_ok), regexp = "All columns.+are present and in the right order")

})

