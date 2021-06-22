#' Custom function for helping generate stop conditions in the bugphyzz package
#'
#' \code{.stop_custom} is a helper function for generating custom stop conditions. This piece of
#' code was taken from Hadley Wickham's Advanced R book, 2nd edition
#' (\url{https://adv-r.hadley.nz/conditions.html#custom-conditions}).
#'
#' @param subclass
#' Character vector of length 1; name of custom error condition.
#' @param message
#' Character vector of length 1; an informative message.
#' @param call
#' Include or not the function call that generates the error; e.g. \code{sys.call(-1)}.
#' @param ...
#' Any other argument useful to identify the source of the error and how to fix it.
#'
#' @return
#' Object of class "error", "condition", and a custom error class name (subclass).
#'
.stop_custom <- function(subclass, message, call = NULL, ...) {
  err <- structure(class = c(subclass, "error", "condition"),
                   list(message = message, call = call, ...))
  stop(err)
}


#' Stop condition for missing required columns in a bugphyzz dataset
#'
#' \code{.stop_required_columns_missing} generates an error condition if one or more of the
#' required columns is missing from a bugphyzz dataset. The required columns can be printed with the
#' \code{validHeaders} function.
#'
#' @param cols
#' Character vector; names of missing columns.
#' @param dataset_name
#' Character vector of length 1; dataset name.
#' @param ...
#' Any other argument useful to identify the source of the error and how to fix it.
#'
#' @return
#' Object of class "required_columns_missing", "error", "condition".
#'
.stop_required_columns_missing <- function(cols, dataset_name, ...){

  cols <- paste0(cols, collapse = ", ")

  if (!is.null(dataset_name)) {
    msg <- paste0(">>> Required columns missing. The following required columns are missing from the `", dataset_name, "` dataset: ", cols, ".",
                  " Please check the required columns with `requiredColumns()`.")
  } else {
    msg <- paste0(">>> Required columns missing. The following required columns are missing: ", cols, ".", " Please check the required columns with `requiredColumns()`")
  }

  .stop_custom(subclass = "required_columns_missing", message = msg,  ...)

}

#' Stop condition for misplaced required columns in a bugphyzz dataset
#'
#' \code{.stop_required_columns_misplaced} generates an error condition if one or more of the
#' required columns is not in the right order in a bugphyzz dataset. The order of required columns
#' can be printed with the \code{validHeaders} function.
#'
#' @param cols
#' Character vector; names of missing columns.
#' @param dataset_name
#' Character vector of length 1; dataset name.
#' @param ...
#' Any other argument useful to identify the source of the error and how to fix it.
#'
#' @return
#' Object of class: "required_columns_misplaced", "error", "condition".
#'
.stop_required_columns_misplaced <- function(cols, dataset_name = NULL, ...) {

  cols <- paste0(cols, collapse = ", ")

  if (!is.null(dataset_name)) {
    msg <- paste0(">>> Misplaced required columns. The following required columns in the `", dataset_name, "` dataset are not in the right place: ", cols, ".",
                  " Please check the correct order with `requiredColumns()`.")
  } else {
    msg <- paste0(">>> Misplaced required columns. The following required columns are not in the right place: ", cols, ".",
                  " Please check the correct order with `requiredColumns()`.")
  }

  .stop_custom(subclass = "required_columns_misplaced", message = msg, ...)
}

#' Stop condition for invalid values in a column of a bugphyzz dataset
#'
#' \code{.stop_invalid_column_values} generates an error condition if a column contains invalid values.
#'
#' @param col
#' Character of length 1; a single column name.
#' @param dataset_name
#' Character of length 1; dataset name.
#' @param n_rows
#' Integer of length 1; number of rows with invalid values.
#' @param values
#' Optional. Invalid values in column. Only the first three will be printed.
#' @param ...
#' Any other argument useful to identify the source of the error and how to fix it.
#'
#' @importFrom utils head
#'
#' @return
#' Object of class "invalid_column_names", "error", "condition".
#'
.stop_invalid_column_values <- function(col, n_rows, dataset_name = NULL, values = NULL, ...) {

  if (!is.null(dataset_name)) {
    msg <- paste0(">>> Invalid values. The column `", col, "` in dataset `", dataset_name,"` contains invalid values in ", n_rows," rows.")
  } else {
    msg <- paste0(">>> Invalid values. The column `", col, "` contains invalid values in ", n_rows," rows.")
  }

  if (!is.null(values)) {
    values <-  paste0(utils::head(as.character(values), n = 3), collapse = ", ")
    # paste0(as.character(utils::head(values, n = 3)), collapse = ", ")
    msg <- paste0(msg, " The first invalid values are: ", values, "...")
  }

  .stop_custom(subclass = "invalid_column_values", message = msg, ...)

}

#' Stop condition for invalid column class (in Attribute_value column)
#'
#' \code{.stop_invalid_column_class} generates an error condition if a column (Attribute_value)
#' is not logical or numeric.
#'
#' @param col_class
#' Character vector of length 1; name of invalid class.
#' @param dataset_name
#' Character vector of length 1; name of the dataset.
#' @param ...
#' Any other argument useful to identify the source of the error and how to fix it.
#'
#' @return
#' Object of class: "invalid_column_class", "error", "condition".
#'
.stop_invalid_column_class <- function(col_class, dataset_name = NULL, ...) {

  if(!is.null(dataset_name)){
    msg <- paste0(">>> Incorrect column class in ", dataset_name, ". The `Attribute_value` column` should be of class logical or numeric, not ", col_class)
  } else {
    msg <- paste0(">>> Incorrect column class. The `Attribute_value` column` should be of class logical or numeric, not ", col_class, "." )
  }

  .stop_custom(subclass = "invalid_column_class", message = msg, ...)

}
