
database <- attribute()

is_attrval_c <- function(x) {
  for (i in seq_along(x)) {
    if ( names(x[i]) == "Attribute_value" && is.character(x[[i]]) ) {
      return(x[[i]])
    }
  }
}

test_that("all headers are present and valid in each data set (
          except for PATRIC and FAC).", {

  valid_headers <- c("NCBI_ID",
                     "Genome_ID",
                     "Accession_number",
                     "Taxon_name",
                     "Attribute",
                     "Attribute_value",
                     "Attribute_source",
                     "Evidence",
                     "Confidence_interval")

  no_names <- c("fatty acid composition", "PATRIC_pathway_human_only")
  db <- database[!(names(database) %in% no_names)]

  for (i in seq_along(db)) {

    actual_headers <- names(db[[i]])
    expect_identical(actual_headers, valid_headers)

  }

})


test_that("all categorical attribute values are present and valid.", {

  y <- lapply(database, function(x) is_attrval_c(x))
  y <- y[!sapply(y, is.null)]
  actual_values <- sort(unique(unlist(y, use.names = FALSE)))

  filename <- system.file("extdata", "attributevalues.tsv",
                          package = "bugphyzz")
  df <- read.table(filename, sep = "\t", header = TRUE)
  valid_values <- sort(unique(df[["attributevalue"]]))

  expect_identical(actual_values, valid_values)

})
