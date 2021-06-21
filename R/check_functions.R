utils::globalVariables(c("attribute"))
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
#'
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
#' \code{checkRequiredColumns} invisibly returns and error condition ("required_columns_missing" or
#' "required_columns_misplaced". It also returns an error message
#' if any of the required columns is missing or is not in the right order. If no errors are found,
#' \code{checkRequiredColumns} prints a message indicating that all required columns are present and
#' in the right order.
#'
#' @importFrom crayon red
#' @importFrom crayon green
#' @importFrom utils capture.output
#'
#' @export
#'
#' @family check functions
#' @seealso
#' \code{\link{requiredColumns}};
#' \code{\link{checkRequiredColumns}};
#' \code{\link{checkRequiredColumnsList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1 - dataset with missing required columns
#'
#' dataset_with_missing_required_columns = data.frame(
#'   NCBI_ID = 1:10,
#'   Attribute = letters[1:10]
#' )
#' err1 <- bugphyzz::checkRequiredColumns(dataset_with_missing_required_columns)
#' err1
#'
#' # Example 2 - dataset with misplaced required columns
#'
#' dataset_with_misplaced_required_columns = data.frame(
#'   NCBI_ID = 1:10,
#'   Taxon_name = letters[1:10],
#'   Genome_ID = 1:10,
#'   Accession_number = 1:10,
#'   Attribute = 1:10,
#'   Attribute_value = 1:10,
#'   Attribute_source = 1:10,
#'   Evidence = 1:10,
#'   Note = 1:10,
#'   Confidence_interval = 1:10
#' )
#'
#' err2 <- bugphyzz::checkRequiredColumns(dataset_with_misplaced_required_columns)
#' err2
#'
#' # Example 3 (dataset wit no errors)
#'
#' dataset_with_required_columns_ok = data.frame(
#'   NCBI_ID = 1:10,
#'   Genome_ID = 1:10,
#'   Accession_number = 1:10,
#'   Taxon_name = letters[1:10],
#'   Attribute = 1:10,
#'   Attribute_value = 1:10,
#'   Attribute_source = 1:10,
#'   Evidence = 1:10,
#'   Confidence_interval = 1:10,
#'   Note = 1:10
#' )
#'
#' bugphyzz::checkRequiredColumns(dataset_with_required_columns_ok)
#'
#' # Example 4 - A bugphyzz dataset
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

    output <- tryCatch(.stop_required_columns_misplaced(cols = misplaced_columns, dataset_name = dataset_name, data_frame_output = data_frame_output, data_frame = data_frame),
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

#' Check required columns across a list of bugphyzz datasets
#'
#' \code{checkRequiredColumnsList} applies the \code{\link{checkRequiredColumns}} function to a list of
#' bugphyzz datasets. If a required column (see \code{\link{requiredColumns}}) is missing or is not in the right order in a dataset,
#' \code{checkRequiredColumnsList} prints a message indicating which columns are missing or must be
#' reordered.
#'
#' @param x
#' A list of bugphyzz datasets.
#'
#' @return
#' \code{checkRequiredColumnsList} invisibly returns a list of error conditions ("required_columns_missing" or
#' "required_columns_misplaced"), and it also prints an error message for each error found.
#' If no errors are found, \code{checkRequiredColumns} prints a message indicating that the required
#' columns are present and in the right order.
#'
#' @export
#'
#' @family check functions
#' @seealso
#' \code{\link{requiredColumns}};
#' \code{\link{checkRequiredColumns}};
#' \code{\link{checkRequiredColumnsList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1 - a list of datasets with errors
#'
#' list_of_datasets <- list(
#'  dataset_with_missing_required_columns = data.frame(
#'    NCBI_ID = 1:10,
#'    Attribute = letters[1:10]
#'  ),
#'
#'  dataset_with_misplaced_required_columns = data.frame(
#'    NCBI_ID = 1:10,
#'    Taxon_name = letters[1:10],
#'    Genome_ID = 1:10,
#'    Accession_number = 1:10,
#'    Attribute = 1:10,
#'    Attribute_value = 1:10,
#'    Attribute_source = 1:10,
#'    Evidence = 1:10,
#'    Note = 1:10,
#'    Confidence_interval = 1:10
#'  ),
#'
#'  dataset_with_required_columns_ok = data.frame(
#'    NCBI_ID = 1:10,
#'    Genome_ID = 1:10,
#'    Accession_number = 1:10,
#'    Taxon_name = letters[1:10],
#'    Attribute = 1:10,
#'    Attribute_value = 1:10,
#'    Attribute_source = 1:10,
#'    Evidence = 1:10,
#'    Confidence_interval = 1:10,
#'    Note = 1:10
#'  )
#' )
#'
#' list_of_errors <- bugphyzz::checkRequiredColumnsList(list_of_datasets)
#' list_of_errors
#'
#' # Example 2 - a list of bugphyzz datasets
#'
#' bp = bugphyzz::physiologies(keyword = "all")
#' list_of_errors_bp = bugphyzz::checkRequiredColumnsList(bp)
#'
#' }
#'
checkRequiredColumnsList <- function(x) {

  output <- vector("list", length(x))
  names(output) <- names(x)

  for ( i in seq_along(x)) {
    dataset <- x[[i]]
    dataset_name = names(x)[i]

    output[[i]] <- checkRequiredColumns(x = dataset, dataset_name = dataset_name)
  }

  output <- purrr::discard(.x = output, is.null)

  return(invisible(output))

}



#' Check column values
#'
#' \code{checkColumnValues} checks if a single column in a data frame contains valid values as
#' according to the template.tsv file.
#'
#' @param column_name
#' A character vector of length 1; name of the column.
#' @param dataset
#' A data frame.
#' @param dataset_name
#' A character vector of length 1; name of the dataset. Default is NULL.
#' @param quiet_success
#' If FALSE, an error message is printed when no errors are found. Default is TRUE.
#'
#' @return
#' \code{checkColumnValues} invisibly returns an error condition (subclass "invalid_column_values" or
#' "invalid_column_class"), and it also prints an error message if the column contains invalid values.
#' If `quiet_success` is FALSE, it prints a message indicating that no errors were found if that is
#' the case.
#'
#' @export
#'
#' @family check functions
#' @seealso
#' \code{\link{checkColumnValues}};
#' \code{\link{checkColumnValuesDF}};
#' \code{\link{checkColumnValuesList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1
#'
#' dataset_with_bad_values <- data.frame(
#'   NCBI_ID = letters[1:10],
#'   Genome_ID = 1:10,
#'   Accession_number = 1:10,
#'   Taxon_name = letters[1:10],
#'   Attribute = 1:10,
#'   Attribute_value = letters[1:10],
#'   Attribute_source = 1:10,
#'   Evidence = 1:10,
#'   Confidence_interval = 1:10,
#'   Note = 1:10
#' )
#'
#' err1 = bugphyzz::checkColumnValues("NCBI_ID",  dataset_with_bad_values)
#' err1
#' err1$bad_values
#'
#' err2 = bugphyzz::checkColumnValues("Attribute", dataset_with_bad_values)
#' err2
#' err2$bad_values
#'
#' err3 = bugphyzz::checkColumnValues("Attribute_value", dataset_with_bad_values)
#' err3
#'
#' # Example 2
#'
#' aerophilicity <- bugphyzz::physiologies(keyword = "aerophilicity")[[1]]
#' aerophilicity_NCBI_ID_errors <- bugphyzz::checkColumnValues("NCBI_ID", aerophilicity) # no errors
#'
#' }
#'
checkColumnValues <- function(column_name, dataset, dataset_name = NULL, quiet_success = TRUE) {

  template <- .template(dataset)
  type_of_test <- template[["test"]][ template[["column_name"]] == column_name ]

  if (type_of_test == "string") {

    column_values <- dataset[[column_name]]
    string <- template[["valid_values"]][ template[["column_name"]] == column_name ]
    values_lgl <- grepl(string, column_values) | is.na(column_values)

    if (!all(values_lgl)) {

      values <- column_values[!values_lgl]

      output_error <- tryCatch(.stop_invalid_column_values(col = column_name, dataset_name = dataset_name, n_rows = length(values), values = values, bad_values = values),
                               invalid_column_values = function(cnd) {
                                 cat(crayon::red(conditionMessage(cnd), "\n"))
                                 cnd
                               })
      return(invisible(output_error))

    }

  } else if (type_of_test == "function") {

    column_values <- dataset[[column_name]]
    function_name  <- template[["valid_values"]][ template[["column_name"]] == column_name ]
    function_output <- paste0("^", paste0(eval(call(function_name)), collapse = "|"), "$")
    values_lgl <- grepl(function_output, column_values) | is.na(column_values)

    if (!all(values_lgl)) {

      values <- column_values[!values_lgl]
      output_error <- tryCatch(.stop_invalid_column_values(col = column_name, dataset_name = dataset_name, n_rows = length(values), values = values, bad_values = values),
                               invalid_column_values = function(cnd) {
                                 cat(crayon::red(conditionMessage(cnd), "\n"))
                                 cnd
                               })
      return(invisible(output_error))

    }

  } else if (type_of_test == "class") {

    column_class <- class(dataset[[column_name]])
    class_options <- template[["valid_values"]][ template[["column_name"]] == column_name ]
    class_lgl <- grepl(class_options, column_class)

    if (!class_lgl) {

      output_error <- tryCatch(.stop_invalid_column_class(col_class = column_class, dataset_name = dataset_name),
                               invalid_column_class = function(cnd) {
                                 cat(crayon::red(conditionMessage(cnd), "\n"))
                                 cnd
                               })
      return(invisible(output_error))

    }
  }


  if (!quiet_success) {

    if (!is.null(dataset_name)) {
      return(message(crayon::green(">>> The column ", column_name, "of the ", dataset_name, "dataset contains valid values.")))
    } else {
      return(message(crayon::green(">>> The column ", column_name, "contains valid values.")))
    }

  }

}

#' Check column values in a data frame
#'
#' \code{checkColumnValuesDF} applies the \code{\link{checkColumnValues}} function to the columns of
#' a single dataset.
#'
#' @param x
#' A data frame.
#' @param dataset_name
#' Character string of length 1; name of the dataset. Default is NULL.
#'
#' @return
#' \code{\link{checkColumnValuesDF}} invisibly returns a list of error conditions (subclass
#' "invalid_column_values" or "invalid_column_class"), and it also prints an error message if
#' a column contains invalid values
#'
#' @export
#'
#' @family check functions
#' @seealso
#' \code{\link{checkColumnValues}};
#' \code{\link{checkColumnValuesDF}};
#' \code{\link{checkColumnValuesList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1
#'
#' dataset_with_bad_values <- data.frame(
#'   NCBI_ID = letters[1:10],
#'   Genome_ID = 1:10,
#'   Accession_number = 1:10,
#'   Taxon_name = letters[1:10],
#'   Attribute = 1:10,
#'   Attribute_value = letters[1:10],
#'   Attribute_source = 1:10,
#'   Evidence = 1:10,
#'   Confidence_interval = 1:10,
#'   Note = 1:10
#' )
#'
#' err <- bugphyzz::checkColumnValuesDF(dataset_with_bad_values)
#' err
#' err$NCBI_ID$bad_values
#'
#' # Example 2
#'
#' aerophilicity <- bugphyzz::physiologies(keyword = "aerophilicity")[[1]]
#' aerophilicity_errors <- bugphyzz::checkColumnValuesDF(aerophilicity)
#' aerophilicity_errors$Evidence$bad_values
#'
#' }
#'
#'
checkColumnValuesDF <- function(x, dataset_name = NULL) {

  output <- purrr::map(colnames(x), ~ checkColumnValues(.x, x, dataset_name = dataset_name)) %>%
    magrittr::set_names(colnames(x)) %>%
    purrr::discard(is.null)
  return(invisible(output))

}

#' Check column values in a list of bugphyzz datasets
#'
#' \code{checkColumnValuesDF} applies the \code{checkColumnValuesDF} function to a list of
#' bugphyzz datasets.
#'
#' @param x
#' A list of bugphyzz datasets.
#'
#' @return
#' \code{checkColumnValuesList} invisibly returns a list of error conditions (subclass
#' "invalid_column_values" or "invalid_column_class"), and it also prints an error message if
#' a column contains invalid values in any of the datasets of the list.
#'
#' @family check functions
#' @seealso
#' \code{\link{checkColumnValues}};
#' \code{\link{checkColumnValuesDF}};
#' \code{\link{checkColumnValuesList}}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1
#'
#' list_of_df <- list(dataset_with_bad_values = data.frame(
#'     NCBI_ID = letters[1:10],
#'     Genome_ID = 1:10,
#'     Accession_number = 1:10,
#'     Taxon_name = letters[1:10],
#'     Attribute = 1:10,
#'     Attribute_value = letters[1:10],
#'     Attribute_source = 1:10,
#'     Evidence = 1:10,
#'     Confidence_interval = 1:10,
#'     Note = 1:10
#'   )
#' )
#'
#' err <- bugphyzz::checkColumnValuesList(list_of_df)
#' err
#'
#' # Example 2
#'
#' bp <- bugphyzz::physiologies(keyword = "all")
#' bp_list_of_errors <- bugphyzz::checkColumnValuesList(bp)
#' bp_list_of_errors$aerophilicity$Evidence$bad_values
#'
#' }
#'
checkColumnValuesList <- function(x) {

  output <- vector("list", length(x))
  names(output) <- names(x)

  for ( i in seq_along(x)) {
    dataset <- x[[i]]
    dataset_name = names(x)[i]

    output[[i]] <- checkColumnValuesDF(x = dataset, dataset_name = dataset_name)
  }

  output <- purrr::discard(.x = output, is.null)

  return(invisible(output))

}

#' Generate a template for a bugphyzz dataset
#'
#' \code{.template} is a helper function for the \code{\link{checkColumnValues}} function.
#' \code{.template} generates a subset of
#' the template.tsv file based on the columns present in a given bugphyzz dataset.
#'
#' A bugphyzz dataset contains a number of mandatory columns (referred to as required columns; see
#' \code{\link{requiredColumns}}), but optionally, it can contain other columns. The purpose of
#' \code{template} is to create a subset of the template.tsv file (which contains all columns
#' of all datasets across bugphyzz) containing only the columns specific for a given bugphyzz dataset.
#'
#' @family helper functions
#' @seealso
#' \code{\link{.template}};
#' \code{\link{.attributes}};
#' \code{\link{checkColumnValues}}
#'
#' @param x
#' A data.frame or tibble; a dataset from bugphyzz.
#'
#' @return
#' A dataset specific template. A tibble with only column names and valid values for a given bugphyzz dataset.
#'
.template <- function(x) {
  template_tsv <- system.file("extdata/template.tsv", package = "bugphyzz")
  template <- readr::read_tsv(template_tsv, col_types = "ccccc")
  template <- template[template$column_name %in% colnames(x),]
  return(template)
}

#' Print valid attributes
#'
#' \code{\link{.attributes}} is a helper function for the \code{\link{checkColumnValues}} function.
#' \code{\link{.attributes}} print the valid attribute names as indicated in the attributes.tsv file.
#'
#' @return
#' A character vector with valid attribute names.
#'
#' @family helper functions
#' @seealso
#' \code{\link{.template}};
#' \code{\link{.attributes}};
#' \code{\link{checkColumnValues}}
#'
.attributes <- function() {
  fname <- system.file("extdata/attributes.tsv", package = "bugphyzz")
  df <- read.table(fname, sep = "\t", header = TRUE, check.names = FALSE)
  attribute_values <- unique(dplyr::pull(.data = df, attribute))
  return(attribute_values)
}
