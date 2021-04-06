
database <- attribute()

is_attrval_c <- function(x) {
  for (i in seq_along(x)) {
    if ( names(x[i]) == "Attribute_value" &&
         is.character(x[[i]])
         # !is.numeric(x[[i]]) &&
         # !is.double(x[[i]])
    )
      return(x[[i]])
  }
}

test_that("all headers are present and valid.", {

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

test_that("all categorical attribute values are present and valid.", {

  y <- lapply(database, function(x) is_attrval_c(x))
  y <- y[-which(sapply(y, is.null))]
  actual_values <- sort(unique(unlist(y, use.names = FALSE)))
  filename <- system.file("extdata", "attributevalues.tsv",
                          package = "bugphyzz")
  df <- read.table(filename, sep = "\t", header = TRUE)
  valid_values <- sort(unique(df[["attributevalue"]]))

  expect_identical(actual_values, valid_values)

})
