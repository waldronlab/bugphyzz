#' Valid headers of bugphyzz data sets
#'
#' \code{valid_headers} contains the valid names and order of the mandatory
#' headers of a bugphyzz data set.
#'
#' @return A character vector with valid header names.
#' @export
#' @importFrom testthat expect
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
    "Attribute_value",
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
#' This name is custom, not taken from the data set itself.
#'
#' @return Error message if the headers are not valid or are not in the correct
#' order. Otherwise, it returns nothing.
#' @export
#'
#' @examples
#' # Example 1
#' \dontrun{
#' library(bugphyzz)
#' datasets <- curationLinks()[, c("physiology", "link")]
#' link1 <- datasets[1, "link"]
#' df1 <- read.csv(link1)
#' expect_valid_headers(df1) # Error expected
#'
#' # Example 2
#' library(bugphyzz)
#' link2 <- datasets[datasets[["physiology"]] == "aerophilicity", "link"]
#' df2 <- read.csv(link2)
#' expect_valid_headers(df2) # No error expected
#' }
expect_valid_headers <- function(x, y = "current" ) {
  # test_output <- all(valid_headers() %in% colnames(x))
  test_output <- identical(valid_headers(),
                           colnames(x)[1:length(valid_headers())])
  expect(test_output == TRUE,
         paste0("Oops! The \"", y, "\" data set does not have ",
         "all of the valid headers or they are not in the correct order."))
}
#' Expect that headers are valid in a list of data frames
#'
#' @param x A named list of valid bugphyzz data frames.
#'
#' @return An error message per each data set that does not pass the test.
#' Otherwise, no output is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' library(bugphyzz)
#' datasets <- curationLinks()[, c("physiology", "link")]
#' database <- vector("list", nrow(datasets))
#' for (i in seq_along(database)) {
#'   names(database)[i] <- datasets[i, "physiology"]
#'   database[[i]] <- read.csv(datasets[i, "link"])
#' }
#' expect_valid_headers_all(database)
#'}
expect_valid_headers_all <- function(x) {
  for (i in seq_along(x)) {
    tryCatch({
      expect_valid_headers(x[[i]], names(x[i]))
      },
      error = function(e) {message(conditionMessage(e))})
  }
}
