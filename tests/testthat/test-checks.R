
test_that("the checkRequiredColumns function works.", {

    list_of_df <- list(

        dataset_with_missing_required_columns = data.frame(
            NCBI_ID = 1:10,
            Attribute = letters[1:10]
      ),

    dataset_with_misplaced_required_columns = data.frame(
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
      ),

    dataset_with_required_columns_ok = data.frame(
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

    )

  err1 <- bugphyzz:::.checkRequiredColumnsDF(
    list_of_df[[1]], dat_name = names(list_of_df)[1]
    )

  err2 <-
    bugphyzz:::.checkRequiredColumnsDF(
      list_of_df[[2]], dat_name = names(list_of_df)[2]
  )

  expect_s3_class(err1, "required_columns_missing")
  expect_s3_class(err2, "required_columns_misplaced")
  expect_message(
    bugphyzz:::.checkRequiredColumnsDF(dataset_with_required_columns_ok),
    regexp = "All columns.+are present and in the right order")

})

test_that("the checkColumn function works.", {

    dataset_with_bad_values <- data.frame(
        NCBI_ID = letters[1:10],
        Genome_ID = 1:10,
        Accession_number = 1:10,
        Taxon_name = letters[1:10],
        Attribute = 1:10,
        Attribute_value = letters[1:10],
        Attribute_source = 1:10,
        Evidence = 1:10,
        Confidence_interval = 1:10,
        Note = 1:10
    )


    err1 <- bugphyzz:::.checkColumnValues(
      "NCBI_ID",  dataset_with_bad_values
      )

    err2 <- bugphyzz:::.checkColumnValues(
      "Attribute", dataset_with_bad_values
      )

    err3 <- bugphyzz:::.checkColumnValues(
      "Attribute_value", dataset_with_bad_values
      )

    expect_s3_class(err1, "invalid_column_values")
    expect_s3_class(err2, "invalid_column_values")
    expect_s3_class(err3, "invalid_column_class")

})

