#' Valid headers of bugphyzz data sets
#'
#' \code{valid_headers} contains the valid names and order of the mandatory
#' headers of a bugphyzz data set.
#'
#' @return A character vector with valid headers names.
#' @export
#'
#' @examples
#' x <- valid_headers()
#' x
valid_headers <- function() {
  c(
    "NCBI_ID",
    "Genome_ID",
    "Accession_number",
    "Taxon_name",
    "Attribute",
    "Attribute_ontology_term",
    "Attribute_value",
    "Attribute_value_ontology_term",
    "Attribute_source",
    "Evidence",
    "Confidence_interval"
    )
  }
#' Expect that headers are valid in a single data frame
#'
#' \code{expect_valid_headers} tests if the first mandatory columns of a
#' bugphyzz data set have valid header names and are in the correct order.
#'
#' @param x A data frame from the bugphyzz database.
#' @param y A character string indicating the name of the data set in bugphyzz.
#'
#' @return Error message if the headers are not valid or are not in the correct
#' order.
#' @export
#'
#' @examples
#' # aerophilicity
#'
expect_valid_headers <- function(x, y = "current" ) {
  # test_output <- all(valid_headers() %in% colnames(x))
  test_output <- identical(valid_headers(),
                           colnames(x)[1:length(valid_headers())])
  expect(test_output == TRUE,
         paste0("Error: The \"", y, "\" data set does not have ",
         "all of the valid headers or they are not in the correct order."))
}
#' Expect that headers are valid in a list of data
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
expect_valid_headers_all <- function(x) {
  for (i in seq_along(x)) {
    tryCatch({
      x_name <- names(x[i])
      message_fun = function(x) paste("The", x, "data set does not contain all of the valid headers!")
      test_output <- all(bugphyzz::valid_headers %in% colnames(x[[i]]))
      expect(test_output == TRUE, message_fun(x_name))
      # testthat::expect_true(test_output,
      #                       label = paste(x_name, "does not contain all of the valid headers!"))
    },
    error = function(e) {message("Oops! ", conditionMessage(e))})
  }
}
