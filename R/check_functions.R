
## Checks for required columns ---------------------------------------------

#' Check required columns
#'
#' \code{.checkRequiredColumns} checks if the required columns
#' (see \code{\link{.requiredColumns}}) are
#' present and in the right order in a single bugphyzz dataset.
#'
#' @param dat A data frame from bugphyzz.
#' @param dat_name A character string indicating the name of the dataset.
#' Default is NULL.
#'
#' @return An error condition of subclass "required_columns_missing" or
#' "required_columns_misplaced". NULL and a message if no errors are found.
#'
#' @importFrom crayon red
#' @importFrom crayon green
#'
#' @family check functions
#' @seealso
#' \code{\link{.requiredColumns}};
#' \code{\link{.stop_required_columns_missing}};
#' \code{\link{.stop_required_columns_misplaced}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsList}}
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies("aerophilicity")[[1]]
#' err <- tryCatch(bugphyzz:::.checkRequiredColumns(x), error = function(e) e)
#' err
#'
#' }
#'
.checkRequiredColumns <- function(dat, dat_name = NULL) {

    ## Check for missing columns
    columns_lgl <- .requiredColumns() %in% colnames(dat)

    if (!all(columns_lgl)) {
        missing_cols <- .requiredColumns()[!columns_lgl]
        .stop_required_columns_missing(missing_cols, dat_name)
    }

    ## Check for misplaced columns
    expected_position <- seq_along(.requiredColumns())
    actual_position <- match(.requiredColumns(), colnames(dat))
    positions_lgl <- expected_position == actual_position

    if (!all(positions_lgl)) {
        misplaced <- ifelse(expected_position != actual_position, "*", "")
        df <- data.frame(expected_position, actual_position, misplaced)
        misplaced_columns <- .requiredColumns()[!positions_lgl]
        .stop_required_columns_misplaced(misplaced_columns, dat_name, df)
    }

    invisible(NULL)
}

#' Check required columns in a bugphyzz dataset
#'
#' \code{.checkRequiredColumnsDF} is a tryCatch wrapper of the
#' \code{.checkRequiredColumns} function. \code{.checkRequiredColumnsDF}
#' checks if the required columns (see \code{\link{.requiredColumns}}) are
#' present and in the right order in a single bugphyzz dataset.
#' If a required column is missing or is not in the right order,
#' it prints a message indicating which columns must be added or reordered.
#'
#' @param dat A data frame from bugphyzz.
#' @param dat_name A character string indicating the name of the dataset.
#' Default is NULL.
#'
#' @return Invisibly returns an error condition
#' ("required_columns_missing" or "required_columns_misplaced" subclass).
#' It also returns an error message if any of the required columns is missing
#' or is not in the right order.
#' If no errors are found, a message indicating that the required
#' columns are present and in the right order is printed.
#'
#' @keywords internal
#'
#' @family check functions
#' @seealso
#' \code{\link{.requiredColumns}};
#' \code{\link{.checkRequiredColumns}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies("aerophilicity")[[1]]
#' err <- bugphyzz:::.checkRequiredColumnsDF(x[[1]])
#'
#' }
#'
.checkRequiredColumnsDF <- function(dat, dat_name = NULL) {

    if (!is.data.frame(dat))
        stop("Not a data.frame. Object of class '", class(dat), "'.",
        " You must provide a data.frame or tibble imported from bugphyzz.",
        call. = FALSE)

    err <- tryCatch(
        required_columns_missing = function(e) {
            message(crayon::red(conditionMessage(e), "\n"))
            e
        },
        required_columns_misplaced = function(e) {
            message(crayon::red(conditionMessage(e), "\n"))
            e
        },
        error = function(e) {
            message(crayon::bgBlue(
                "Error in the", dat_name, "dataset. ", conditionMessage(e), "\n"
            ))
            e
        },
        .checkRequiredColumns(dat, dat_name)
    )

    if (!length(err)) {
        if (!is.null(dat_name)) {
            message(crayon::green(
                ">>> All columns in the `", dat_name,
                "` dataset are present and in the right order.", sep = ""
            ))
            return(invisible(NULL))
        } else {
            message(crayon::green(
                ">>> All columns are present and in the right order."
            ))
            return(invisible(NULL))
        }
    }

    return(invisible(err))
}

#' Check required columns across a list of bugphyzz datasets
#'
#' \code{.checkRequiredColumnsList} applies the
#' \code{\link{.checkRequiredColumnsDF}} function to a list of
#' bugphyzz datasets. If a required column (see \code{\link{.requiredColumns}})
#' is missing or is not in the right order in a dataset,
#' \code{.checkRequiredColumnsList} prints a message indicating which columns
#' are missing or must be reordered.
#'
#' @param list A list of bugphyzz datasets.
#' @param table If TRUE (default), it returns a table instead of a list.
#'
#' @return Invisibly returns a list of error conditions
#' ("required_columns_missing" or "required_columns_misplaced" subclasses),
#' and it also prints an error message for each error found.
#' If no errors are found, a message indicating that the required
#' columns are present and in the right order is printed. If argument
#' `table=TRUE`, it returns a data frame instead of a list.
#'
#' @importFrom purrr map2
#' @importFrom purrr discard
#' @importFrom purrr map
#' @importFrom magrittr set_colnames
#'
#' @family check functions
#' @seealso
#' \code{\link{.requiredColumns}};
#' \code{\link{.checkRequiredColumns}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsList}}
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies()
#' list_of_errors <- bugphyzz:::.checkRequiredColumnsList(x)
#' list_of_erros$length
#'
#' }
#'
.checkRequiredColumnsList <- function(list, table = TRUE) {

    if (class(list) != "list")
        stop("Not a list. Object of class '", class(list), "'.",
            " Provide a list of data frames imported with bugphyzz functions.",
            call. = FALSE)

    err <- purrr::map2(list, names(list), ~ .checkRequiredColumnsDF(.x, .y)) %>%
        purrr::discard(is.null)

    if (!length(err))
      return(invisible(NULL))

    if (table) {
        err_table <- err %>%
            purrr::map(~ c(class(.x)[1], .x$cols)) %>%
            do.call(rbind, .) %>%
            tibble::as_tibble(rownames = "dataset", .name_repair = "unique") %>%
            suppressMessages() %>% # suppress message from .name_repair
            magrittr::set_colnames(c("dataset", "error_type", "columns")) %>%
            dplyr::mutate(
                error_type = sub("required_columns_", "", error_type)
            ) %>%
            .appendLinks()
        return(err_table)
    }

    return(invisible(err))
}

## Checks for column values ------------------------------------------------

#' Check column values
#'
#' \code{.checkColumnValues} checks if a single column in a data frame contains
#' valid values according to the extdata/template.tsv file.
#'
#' @param col A character string indicating the name of the column.
#' @param dat A data frame imported through the bugphyzz functions.
#' @param dat_name A character string indicating the name of the dataset.
#' Default is NULL.
#' @param quiet_success If FALSE, an error message is printed when no errors
#' are found. Default is TRUE.
#'
#' @return An error condition of subclass "invalid_column_values" or
#' "invalid_column_class". If `quiet_success` is FALSE and no errors were
#' found, it returns NULL and prints a message indicating that no errors were
#' found.
#'
#' @family check functions
#' @seealso
#' \code{\link{.checkColumnValues}};
#' \code{\link{.checkColumnValuesDF}};
#' \code{\link{.checkColumnValuesList}}
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies("aerophilicity")[[1]]
#' err <- tryCatch(bugphyzz:::.checkColumnValues(x), error = function(e) e)
#'
#' }
#'
.checkColumnValues <-
    function(col, dat, dat_name = NULL, quiet_success = TRUE) {

        ## Check that the column is in template
        template <- .template(dat)

        if (!col %in% template[["column_name"]])
            .stop_uncatalogued_column(col, dat_name)

        ## Check column values based on the "value_test" column of the
        ## extdata/template.tsv file
        type_of_test <-
            template[["value_test"]][template[["column_name"]] == col]

        if (type_of_test == "string") {
            col_values <- dat[[col]]
            string <-
                template[["valid_values"]][template[["column_name"]] == col]
            values_lgl <- grepl(string, col_values) | is.na(col_values)

            if (!all(values_lgl)) {
                invalid_values <- col_values[!values_lgl]
                n_rows <- length(invalid_values)
                invalid_pos <- seq_along(col_values)[!values_lgl]

                .stop_invalid_column_values(
                    col, n_rows, dat_name, invalid_values,
                    invalid_pos = invalid_pos
                )
            }
        } else if (type_of_test == "function") {
            col_values <- dat[[col]]
            fun  <-
                template[["valid_values"]][template[["column_name"]] == col]
            fun_regex <-
                paste0("^(", paste0(eval(call(fun)), collapse = "|"), ")$")
            values_lgl <- grepl(fun_regex, col_values) | is.na(col_values)

            if (!all(values_lgl)) {
                invalid_values <- col_values[!values_lgl]
                n_rows <- length(invalid_values)
                invalid_pos <- seq_along(col_values)[!values_lgl]

                .stop_invalid_column_values(
                    col, n_rows, dat_name, invalid_values,
                    invalid_pos = invalid_pos
                )
            }
        }
}

#' Check column values in a data frame
#'
#' \code{.checkColumnValuesDF} applies the \code{\link{.checkColumnValues}}
#' function to all of the columns of a single dataset. It's a tryCatch
#' wrapper of \code{\link{.checkColumnValues}}.
#'
#' @param dat A data frame.
#' @param dat_name Character string indicating the name of the dataset.
#' Default is NULL.
#'
#' @return Invisibly returns a list of error
#' conditions (subclass "invalid_column_values" or "invalid_column_class"),
#' and it also prints an error message if a column contains invalid values.
#' If no errors are found, it returns NULL and a message indicating it.
#'
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr discard
#'
#' @keywords internal
#'
#' @family check functions
#' @seealso
#' \code{\link{.checkColumnValues}};
#' \code{\link{.checkColumnValuesDF}};
#' \code{\link{.checkColumnValuesList}}
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies("aerophilicity")[[1]]
#' err <- bugphyzz:::.checkColumnValuesDF(x)
#'
#' }
#'
.checkColumnValuesDF <- function(dat, dat_name = NULL) {

    if (!is.data.frame(dat))
        stop("Not a data.frame. Object of class '", class(dat), "'.",
        " You must provide a data.frame or tibble imported from bugphyzz.",
        call. = FALSE)

    col_names <- colnames(dat)

    err <- purrr::map(col_names, ~{
        tryCatch(
            uncatalogued_column = function(e) {
                message(crayon::red(conditionMessage(e), "\n"))
                e
            },
            invalid_column_class = function(e) {
                message(crayon::red(conditionMessage(e), "\n"))
                e
            },
            invalid_column_values = function(e) {
                message(crayon::red(conditionMessage(e), "\n"))
                e
            },
            error = function(e) {
                message(crayon::bgBlue(
                    "Error in the", dat_name, "dataset. ",
                    conditionMessage(e), "\n"
                ))
                e
            },
            .checkColumnValues(.x, dat, dat_name, quiet_success = TRUE)
        )
    }) %>%
        purrr::set_names(col_names) %>%
        purrr::discard(is.null)

    if (!length(err)) {
        if (!is.null(dat_name)) {
            message(crayon::green(
                "All values are valid in the ", dat_name, "dataset."
            ))
          return(invisible(NULL))
        } else if (is.null(dat_name)) {
            message(crayon::green(
                "All values are valid in the current dataset."
            ))
          return(invisible(NULL))
        }
    }

    return(invisible(err))
}

#' Check column values in a list of bugphyzz datasets
#'
#' \code{.checkColumnValuesList} applies the \code{.checkColumnValuesDF}
#' function to a list of bugphyzz datasets.
#'
#' @param list A list of bugphyzz datasets.
#' @param table If TRUE (default), it returns a table instead of a list.
#'
#' @return Invisibly returns a list of error conditions (subclass
#' "invalid_column_values" or "invalid_column_class"), and it also prints
#' an error message if a column contains invalid values in any of the datasets
#' in the list. If no errors are found, it returns NULL and prints a message
#' indicating it (per datasest). If `table = TRUE`, it returns a tibble of
#' errors.
#'
#' @importFrom purrr map2
#' @importFrom purrr set_names
#' @importFrom purrr discard
#'
#' @family check functions
#' @seealso
#' \code{\link{.checkColumnValues}};
#' \code{\link{.checkColumnValuesDF}};
#' \code{\link{.checkColumnValuesList}}
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies()
#' list_of_errors <- bugphyz:::.checkColumnValuesList(x)
#' list_of_errors
#'
#' }
#'
.checkColumnValuesList <- function(list, table = TRUE) {

    if (class(list) != "list")
        stop("Not a list. Object of class '", class(list), "'.",
            " Provide a list of data frames imported with bugphyzz functions.",
            call. = FALSE)

    dats_names <- names(list)

    err <- purrr::map2(list, dats_names, ~ {.checkColumnValuesDF(.x, .y)}) %>%
        purrr::set_names(dats_names) %>%
        purrr::discard(is.null)

    if (!length(err))
        return(invisible(NULL))

    if (table) {
        err_table <- err %>%
            .err_list_to_table() %>%
            .appendLinks()
        return(err_table)
    }

    return(invisible(err))
}

# Stop functions ---------------------------------------------------

#' Function for helping to generate custom stop (error) conditions
#'
#' \code{.stop_custom} is a helper function for generating custom stop (error)
#' conditions. This function was taken from
#' Hadley Wickham's Advanced R book, 2nd edition
#' (\url{https://adv-r.hadley.nz/conditions.html#custom-conditions}).
#' This function should be used within custom stop functions.
#'
#' @param subclass A character string indicating the name of the custom error.
#' @param message A character string with an informative message.
#' @param call Include or not the function call that generates the error;
#' e.g., \code{sys.call(-1)}.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition. Object of class "error", "condition",
#' and a custom error class name (subclass).
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_required_columns_missing}};
#' \code{\link{.stop_required_columns_misplaced}}
#' \code{\link{.stop_invalid_column_values}};
#' \code{\link{.stop_uncatalogued_column}};
#'
.stop_custom <- function(subclass, message, call = NULL, ...) {
    err <- structure(
        list(message = message, call = call, ...),
        class = c(subclass, "error", "condition")
    )
    stop(err)
}

#' Stop condition for missing required columns in a bugphyzz dataset
#'
#' \code{.stop_required_columns_missing} generates an error condition of
#' class "required_columns_missing".
#' The required columns can be printed with the \code{\link{.requiredColumns}}
#' function. This function should be used within the
#' \code{\link{.checkRequiredColumns}} function.
#'
#' @param cols A character vector containing the names of missing columns.
#' @param dat_name A character string indicating the name of the dataset.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition.
#' Object of class "required_columns_missing", "error", "condition".
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_required_columns_missing}};
#' \code{\link{.stop_required_columns_misplaced}};
#' \code{\link{.checkRequiredColumns}}
#'
.stop_required_columns_missing <- function(cols, dat_name = NULL, ...) {
    cols <- paste0(cols, collapse = ", ")
    if (!is.null(dat_name)) {
        msg <- paste0(">>> Required columns missing.",
            " The following required columns are missing from the `",
            dat_name, "` dataset: ", cols, ".",
            " Required columns can be checked with ",
            " `bugphyzz:::.requiredColumns()`.")
    } else {
        msg <- paste0(">>> Required columns missing.",
            " The following required columns are missing: ", cols, ".",
            " Required columns can be checked with",
            " `bugphyzz:::.requiredColumns()`")
    }
    .stop_custom(subclass = "required_columns_missing", message = msg,
        cols = cols, dat_name = dat_name, ...)
}

#' Stop condition for misplaced required columns in a bugphyzz dataset
#'
#' \code{.stop_required_columns_misplaced} generates an error condition of
#' class "required_columns_misplaced".
#' The order of the required columns can be printed with the
#' \code{\link{.requiredColumns}} function. This function should be used within
#' the \code{\link{.checkRequiredColumns}} function.
#'
#' @param cols A character vector containing the names of the missing columns.
#' @param dataset_name A character string indicating the name of the dataset.
#' @param df A data frame with information about the expected and actual
#' positions of the required columns in a bugphyzz dataset. See
#' \code{\link{.checkRequiredColumns}}.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition.
#' Object of class: "required_columns_misplaced", "error", "condition".
#'
#' @importFrom utils capture.output
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_required_columns_missing}};
#' \code{\link{.stop_required_columns_misplaced}};
#' \code{\link{.checkRequiredColumns}}
.stop_required_columns_misplaced <- function(cols, dat_name = NULL, df, ...) {
    df_print <- paste0(utils::capture.output(df), collapse = "\n")
    cols <- paste0(cols, collapse = ", ")
    if (!is.null(dat_name)) {
        msg <- paste0(">>> Misplaced required columns.",
            " The following required columns in the `", dat_name,
            " ` dataset are not in the right place: ", cols, ".",
            " The right order of the required columns can be checked ",
            " with `bugphyzz:::.requiredColumns()`.",
            " More info: ", "\n\n", df_print)
    } else {
        msg <- paste0(">>> Misplaced required columns.",
            " The following required columns are not in the",
            " right place: ", cols, ".",
            " The right order of the required columns can be checked ",
            " with `bugphyzz:::.requiredColumns()`.",
            " More info: ", "\n\n", df_print)
    }
    .stop_custom(subclass = "required_columns_misplaced", message = msg,
         cols = cols, dat_name = dat_name, df = df, ...)
}

#' Stop condition for invalid values in a column of a bugphyzz dataset
#'
#' \code{.stop_invalid_column_values} generates an error condition of class
#' "invalid_column_names". This function should be used within the
#' \code{\link{.checkColumnValues}} function.
#'
#' @param col Character string containing a single column name.
#' @param dat_name Character string indicating the dataset name.
#' @param n_rows Integer of length 1; number of rows with invalid values.
#' @param values Optional.
#' Invalid values in column. Only the first three will be printed.
#'
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @importFrom utils head
#'
#' @return
#' Error condition.
#' Object of class "invalid_column_names", "error", "condition".
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_invalid_column_values}};
#' \code{\link{.stop_invalid_column_class}};
#' \code{\link{.stop_uncatalogued_column}};
#' \code{\link{.checkColumnValues}}
#'
.stop_invalid_column_values <-
    function(col, n_rows, dat_name = NULL, values = NULL, ...) {
        if (!is.null(dat_name)) {
            msg <- paste0(
                ">>> Invalid values. The column `", col, "` in dataset `",
                dat_name,"` contains invalid values in ", n_rows," rows."
            )
        } else {
            msg <- paste0(
                ">>> Invalid values. The column `", col,
                "` contains invalid values in ", n_rows," rows."
            )
        }
        if (!is.null(values)) {
            head_values <- paste0(
                utils::head(as.character(values), n = 3), collapse = "; "
            )
            msg <- paste0(
              msg, " The first invalid values are: ", head_values, "..."
            )
        }
        .stop_custom(
            subclass = "invalid_column_values", message = msg,
            col = col, n_rows = n_rows, dat_name = dat_name,
            invalid_values = values, ...
        )
}

#' Stop condition for an uncatalogued column
#'
#' \code{.stop_uncatalogued_column} returns an error condition of class
#' "uncatalogued_column".
#' catalogued in the extdata/template.csv file. This function shold be used
#' within the \code{\link{.checkColumnValues}} function.
#'
#' @param col Character string indicating the column name.
#' @param dat_name Character string inticating the dataset name.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition.
#' Object of class: "uncatalogued_column", "error", "condition".
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_invalid_column_values}};
#' \code{\link{.stop_invalid_column_class}};
#' \code{\link{.stop_uncatalogued_column}};
#' \code{\link{.checkColumnValues}}
#'
.stop_uncatalogued_column <- function(col, dat_name = NULL, ...) {
    if (!is.null(dat_name)) {
        msg <- paste0(
            ">>> Uncatalogued column. The column ", col, " of the dataset ",
            dat_name, " cannot be checked because it's not included in the",
            " template file. Please add it to extdata/template.tsv"
        )
    } else {
        msg <- paste0(
            ">>> Uncatalogued column. The column ", col,
            " cannot be checked because it's not included in the template",
            " file. Please add it to extdata/template.tsv."
        )
    }
    .stop_custom(subclass = "uncatalogued_column", message = msg,
      col = col, dat_name = dat_name, ...)
}

# Helper functions --------------------------------------------------------

#' Required columns
#'
#' \code{.requiredColumns} prints the names and order of the columns that must
#' be present in all of the bugphyzz data sets.
#'
#' @return A character vector with mandatory column names (required columns).
#'
#' @seealso
#' \code{\link{.checkRequiredColumns}}
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#' bugphyzz:::.requiredColumns()
#' }
#'
.requiredColumns <- function() {
    fname <- system.file("extdata/template.tsv", package = "bugphyzz")
    df <- utils::read.table(fname, sep = "\t", header = TRUE)
    df <- df[df[["requiredness"]] == "required",]
    df[order(df[["required_column_order"]]), "column_name"]
}

#' Generate a template for a bugphyzz dataset
#'
#' \code{.template} is a helper function for the
#' \code{\link{.checkColumnValues}} function. \code{.template} generates a
#' subset of the extdata/template.tsv file based on the columns present in a
#' given bugphyzz dataset.
#'
#' A bugphyzz dataset contains a number of mandatory columns (referred to as
#' required columns; see \code{\link{.requiredColumns}}), but optionally, it can
#' contain other columns. The purpose of \code{template} is to create a subset
#' of the extdata/template.tsv file containing only the columns specific for a
#' given bugphyzz dataset.
#'
#' @importFrom utils read.table
#'
#' @seealso
#' \code{\link{.checkColumnValues}}
#'
#' @param dataset A data frame or tibble imported from bugphyzz.
#'
#' @return A dataset specific template.
#' A data frame with only column names and valid values for a given bugphyzz
#' dataset.
#'
#' @keywords internal
#'
.template <- function(dataset) {
    template_tsv <- system.file("extdata/template.tsv", package = "bugphyzz")
    template <- utils::read.table(
        file = template_tsv, sep = "\t", check.names = FALSE, header = TRUE
    )
    template[template[["column_name"]] %in% colnames(dataset), ]
}

#' Print valid attributes
#'
#' \code{\link{.attributes}} is a helper function for the
#' \code{\link{.checkColumnValues}} function. \code{\link{.attributes}} print
#' the valid attribute names as indicated in the attributes.tsv file.
#'
#' @return A character vector with valid attribute names.
#'
#' @importFrom utils read.table
#'
#' @seealso
#' \code{\link{.checkColumnValues}}
#'
#' @keywords internal
#'
.attributes <- function() {
    fname <- system.file("extdata/attributes.tsv", package = "bugphyzz")
    df <- utils::read.table(
        fname, sep = "\t", header = TRUE, check.names = FALSE
    )
    unique(df[,"attribute"])
}

#' Append links to error table
#'
#' \code{appendLinks} appends the links of each dataset with errors when
#' `table = TRUE` is set in the calls to
#' \code{\link{.checkRequiredColumnsList}} and
#' \code{\link{.checkColumnValuesList}}
#'
#' @param x A data frame or tibble.
#'
#' @return A tibble.
#' @keywords internal
#'
#' @seealso
#' \code{\link{.checkRequiredColumnsList}};
#' \code{\link{.checkColumnValuesList}}
#'
.appendLinks <- function(x) {
    select_cols <- c("physiology", "link")
    phys_links <- curationLinks() %>%
        dplyr::select(tidyselect::all_of(select_cols))
    custom_links <- customLinks() %>%
        dplyr::select(tidyselect::all_of((select_cols)))
    links <- dplyr::bind_rows(phys_links, custom_links)
    x %>%
        dplyr::left_join(links, by = c("dataset" = "physiology"))
}


#' Error list to table
#'
#' \code{.err_list_to_table} converts a list of errors generated with the
#' \code{\link{.checkColumnValuesList}} function into a tibble.
#'
#' @param err List of errors.
#'
#' @return A tibble.
#'
#' @importFrom purrr modify_depth
#' @importFrom purrr set_names
#' @importFrom purrr discard
#' @importFrom methods as
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{.checkColumnValuesList}}
#'
.err_list_to_table <- function(err) {
    err %>%
        purrr::modify_depth(.depth = 2, ~{
            x <-
                methods::as(.x, "list") %>%
                purrr::set_names(names(.x)) %>%
                purrr::discard(is.null) %>%
                dplyr::bind_rows()

            col_names_x <- colnames(x)
            if ("invalid_values" %in% colnames(x)) {
                x <- dplyr::mutate(
                    x, dplyr::across(.data[["invalid_values"]], as.character)
                )
            }
            x
        }) %>%
        purrr::modify_depth(.depth = 1, ~{
            dplyr::bind_rows(.x)
        }) %>%
        dplyr::bind_rows(.id = "dataset") %>%
        dplyr::group_by(
            dplyr::across(
                c(-.data[["invalid_values"]], -.data[["invalid_pos"]])
            )
        ) %>%
        dplyr::summarise(
            dplyr::across(tidyselect::starts_with("invalid_"), ~list(.x))
        ) %>%
        dplyr::mutate(
            message = sub(">>> (\\w+ \\w+)\\..+$", "\\1", message)
        ) %>%
        dplyr::rename(error_type = message) %>%
        dplyr::select(-.data[["dat_name"]])
}

