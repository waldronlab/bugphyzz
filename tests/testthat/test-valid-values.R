
database <- attribute()

test_that("headers are valid.", {

  x <- lapply(database, function(x) names(x))
  actual_headers <- sort(unique(unlist(x, use.names = FALSE)))
  valid_headers <- sort(unique(c("NCBI_ID",
                     "Genome_ID",
                     "Accession_number",
                     "Taxon_name",
                     "Attribute",
                     "Attribute_ontology_term",
                     "Attribute_value",
                     "Attribute_value_ontology_term",
                     "Attribute_source",
                     "Evidence",
                     "Confidence_interval")))

  expect_identical(actual_headers, valid_headers)

  })

test_that("attribute values are valid.", {

  y <- lapply(database, function(x) x[["Attribute_value"]])
  actual_values <- sort(unique(unlist(y, use.names = FALSE)))
  filename <- system.file("extdata", "attributevalues.tsv",
                          package = "bugphyzz")
  df <- read.table(filename, sep = "\t", header = TRUE)
  valid_values <- sort(unique(df[["attributevalue"]]))

  expect_identical(actual_values, valid_values)

})
