test_that("attribute values are valid.", {

  x <- attribute()
  y <- lapply(x, function(x) x[["Attribute_value"]])

  # TODO - Function to test all data frames in y instead of one by one.

  # Oxygen ------------------------------------------------------------------
  # Valid values for oxygen
  oxygen_filename <- system.file("extdata", "attributevalues.tsv", package = "bugphyzz")
  oxygen_valid_df <- read.table(oxygen_filename, sep = "\t", header = TRUE)
  oxygen_valid_values <- sort(unique(oxygen_valid_df[["attributevalue"]]))
  # Test values for oxygen
  oxygen_test_values <- sort(unique(y[["oxygen"]]))
  # Testing
  expect_identical(oxygen_test_values, oxygen_valid_values)

})
