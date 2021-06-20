
#' Required columns
#'
#' \code{requiredColumns} prints the names and order of the columns that must be present in all
#' bugphyzz data sets.
#'
#' @return A character vector with mandatory column names (required columns).
#'
#' @export
#'
#' @seealso
#' \code{\link{requiredColumns}};
#' \code{\link{checkRequiredColumns}};
#' \code{\link{checkRequiredColumnsList}}
#'
#' @examples
#' x <- requiredColumns()
#' x
requiredColumns<- function() {
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

#' Check required columns across a list of bugphyzz datasets
#'
#' \code{checkRequiredColumnsList} applies the \code{\link{checkRequiredColumns}} function to a list of
#' bugphyzz datasets. If a required column is missing or is not in the right order, it prints a
#' message indicating which columns must be added or reordered.
#'
#' @param x
#' A list of bugphyzz datasets.
#'
#' @return
#' An error message if any of the required columns is missing or is not in the right order.
#' Otherwise, it returns a message indicating that the required columns are present and in the right order.
#'
#' @export
#'
#' @seealso
#' \code{\link{requiredColumns}};
#' \code{\link{checkRequiredColumns}};
#' \code{\link{checkRequiredColumnsList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' bp = bugphyzz::physiologies(keyword = "all")
#' bugphyzz::checkRequiredColumnsList(bp)
#'
#' }
#'
checkRequiredColumnsList <- function(x) {

  for (i in seq_along(x)) {

    dataset = x[[i]]
    dataset_name = names(x)[i]

    checkRequiredColumns(x = dataset, dataset_name = dataset_name )
  }

}

#' Check required columns in a single bugphyzz dataset
#'
#' \code{checkRequiredColumns} checks if the required columns (see \code{\link{requiredColumns}}) are
#' present and in the right order in a single bugphyzz dataset. If a required column is missing or
#' is not in the right order, it prints a message indicating which columns must be added or reordered.
#'
#' @param x
#' A dataset from bugphyzz
#'
#' @param dataset_name
#' Character of length 1; name of dataset. Default is NULL.
#'
#' @return
#' An error message if any of the required columns is missing or is not in the right order.
#' Otherwise, it returns a message indicating that required columns are present and in the right order.
#'
#' @importFrom crayon red
#' @importFrom crayon green
#' @importFrom utils capture.output
#'
#' @export
#'
#' @seealso
#' \code{\link{requiredColumns}};
#' \code{\link{checkRequiredColumns}};
#' \code{\link{checkRequiredColumnsList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' oxygen <- bugphyzz::physiologies(keyword = "aerophilicity")
#' bugphyzz::checkRequiredColumns(x = oxygen[[1]], dataset_name = names(oxygen)[1])
#'
#' }
#'
#'
checkRequiredColumns <- function(x, dataset_name = NULL) {

  # Check if any of the required columns is missing
  columns_lgl <- requiredColumns() %in% colnames(x)

  if (!all(columns_lgl)) {

    missing_cols <- requiredColumns()[!columns_lgl]
    output <- tryCatch(.stop_required_columns_missing(cols = missing_cols, dataset_name = dataset_name),
                       required_columns_missing = function(cnd) {
                         cat(crayon::red(conditionMessage(cnd), "\n"))
                         cnd})
    return(invisible(output))
  }

  # Check if the required columns are in the correct order
  first_columns <- colnames(x)[seq_along(requiredColumns())]
  columns_lgl <- requiredColumns() == first_columns

  if (!all(columns_lgl)) {

    misplaced_columns <- requiredColumns()[!columns_lgl]

    required_column = requiredColumns()
    expected_position = seq_along(requiredColumns())
    actual_position = purrr::map_int(requiredColumns(), ~ grep(paste0("^", .x, "$"), colnames(x)))
    misplaced = ifelse(expected_position != actual_position, "*", "")

    data_frame <- data.frame(required_column, expected_position, actual_position, misplaced)
    data_frame_output <- paste0(utils::capture.output(data_frame), collapse = "\n")

    output <- tryCatch(.stop_required_columns_misplaced(cols = misplaced_columns, dataset_name = dataset_name, data_frame_output = data_frame_output),
             required_columns_misplaced = function(cnd) {
               cat(crayon::red(conditionMessage(cnd), "\n", "The misplaed columns are:", "\n", cnd$data_frame_output, "\n"))
               cnd})
    return(invisible(output))
  }

  if (!is.null(dataset_name)) {
    return(message(crayon::green(">>> All columns in the `", dataset_name, "` dataset are present and in the right order.", sep = "")))
  } else {
    return(message(crayon::green(">>> All columns are present and in the right order.")))
  }

}

#' Check if values are valid across the columns of a list of bugphyzz datasets
#'
#' \code{checkColumnValuesList} applies the \code{checkColumnValues} function to a list of dataframes
#' imported from the bugphyzz package. The \code{checkColumnValues} function prints a message if
#' columns don't have valid values as specified in the template.tsv file.
#'
#' @param x
#' A named list of datasets in bugphyzz.
#'
#' @return
#' Prints a message on screen if invalid values are found in one or more columns of each dataset.
#'
#' @export
#'
#' @seealso \code{\link{checkColumnValues}}; \code{\link{checkColumnValuesList}}
#'
#' @examples
#' \dontrun{
#'
#' bp <- bugphyzz::physiologies(keyword = "all")
#' bugphyzz::checkColumnValuesList(bp)
#'
#' }
#'
checkColumnValuesList <- function(x) {

  for (i in seq_along(x)) {

    dataset = x[[i]]
    dataset_name = names(x)[i]

    checkColumnValues(x = dataset, dataset_name = dataset_name )
  }

}

#' Check if values are valid across the columns of a bugphyzz dataset
#'
#' \code{checkColumnValues} compares the values in each column of a bugphyzz dataset with
#' the set of valid values or characters defined in the `valid_values` column in the template.tsv
#' file, and prints an error message to screen if invalid values are found in a column.
#'
#' @param x
#' A dataset from bugphyzz, e.g. aerophilicity.
#' @param dataset_name
#' Character vector of length 1; optional name for the dataset.
#'
#' @return
#' Prints a message on screen if invalid values are found in one or more columns.
#'
#' @importFrom crayon red
#'
#' @export
#'
#' @seealso
#' \code{\link{checkColumnValues}}; \code{\link{checkColumnValuesList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' oxygen <- bugphyzz::phyisiologies(keyword = "aerophilicity")
#' bugphyzz::checkColumnValues(x = oxygen[[1]], dataset_name = names(oxygen)[1])#'
#'
#' }
#'
checkColumnValues <- function(x, dataset_name = NULL) {

  template <- .template(x)
  column_name <- template[["column_name"]]
  valid_values <- template[["valid_values"]]

  for (i in seq_along(column_name)) {
    values_lgl <- grepl(valid_values[i], x[[ column_name[i] ]]) | is.na(x[[ column_name[i] ]])
    values <- x[[ column_name[i] ]][!values_lgl]
    if (!all(values_lgl)) {
      tryCatch(.stop_invalid_column_values(col = column_name[i], n_rows = sum(!values_lgl), dataset_name = dataset_name, values = values),
               invalid_column_values = function(cnd)  cat(crayon::red(conditionMessage(cnd), "\n")))
    }
  }
}

#' Generates a template for a bugphyzz dataset
#'
#' @param x
#' A data.frame or tibble; a dataset from bugphyzz.
#'
#' @return
#' A tibble with column names and valid values for a given bugphyzz dataset.
#'
.template <- function(x) {
  template_tsv <- system.file("extdata/template.tsv", package = "bugphyzz")
  template <- readr::read_tsv(template_tsv, col_types = "ccccc")
  template <- template[template$column_name %in% colnames(x),]
  return(template)
}
