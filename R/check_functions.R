
# Check functions ---------------------------------------------------------

#' Check required columns in a single bugphyzz dataset
#'
#' \code{.checkRequiredColumnsDF} checks if the required columns
#' (see \code{\link{.requiredColumns}}) are
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
#' @importFrom crayon red
#' @importFrom crayon green
#'
#' @family check functions
#' @seealso
#' \code{\link{.requiredColumns}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsDFList}};
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies("aerophilicity")[[1]]
#' err <- tryCatch(bugphyzz:::.checkRequiredColumnsDF(x), error = function(e) e)
#' err
#'
#' }
#'
.checkRequiredColumnsDF <- function(dat, dat_name = NULL) {

    if (!is.data.frame(dat))
        stop("Not a data.frame. Object of class '", class(dat), "'.",
            " You must provide a data.frame or tibble imported from bugphyzz.",
            call. = FALSE)

    columns_lgl <- .requiredColumns() %in% colnames(dat)

    if (!all(columns_lgl)) {

        missing_cols <- .requiredColumns()[!columns_lgl]
        .stop_required_columns_missing(missing_cols, dat_name)

    }

    expected_position <- seq_along(.requiredColumns())
    actual_position <- match(.requiredColumns(), colnames(dat))

    positions_lgl <- expected_position == actual_position

    if (!all(positions_lgl)) {

        misplaced <- ifelse(expected_position != actual_position, "*", "")
        df <- data.frame(expected_position, actual_position, misplaced)

        misplaced_columns <- .requiredColumns()[!positions_lgl]

        .stop_required_columns_misplaced(misplaced_columns, dat_name, df)

    }

    if (!is.null(dat_name)) {

        message(crayon::green(
            ">>> All columns in the `", dat_name,
            "` dataset are present and in the right order.", sep = ""
        ))

    } else {

        message(crayon::green(
            ">>> All columns are present and in the right order."
        ))

    }

}

#' Check required columns across a list of bugphyzz datasets
#'
#' \code{.checkRequiredColumnsDFList} applies the
#' \code{\link{.checkRequiredColumnsDF}} function to a list of
#' bugphyzz datasets. If a required column (see \code{\link{.requiredColumns}})
#' is missing or is not in the right order in a dataset,
#' \code{.checkRequiredColumnsDFList} prints a message indicating which columns
#' are missing or must be reordered.
#'
#' @param list A list of bugphyzz datasets.
#'
#' @return Invisibly returns a list of error conditions
#' ("required_columns_missing" or "required_columns_misplaced" subclasses),
#' and it also prints an error message for each error found.
#' If no errors are found, a message indicating that the required
#' columns are present and in the right order is printed.
#'
#' @importFrom purrr map2
#' @importFrom purrr discard
#'
#' @family check functions
#' @seealso
#' \code{\link{.requiredColumns}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsDFList}};
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- physiologies()
#' list_of_errors <- bugphyzz:::.checkRequiredColumnsDFList(x)
#' list_of_erros$length
#'
#' }
#'
.checkRequiredColumnsDFList <- function(list) {

    if (class(list) != "list")
        stop("Not a list. Object of class '", class(list), "'.",
            " Provide a list of data frames imported with bugphyzz functions.",
            call. = FALSE)

    purrr::map2(list, names(list), ~ {

        tryCatch(
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
                    "Error in the", .y, "dataset. ", conditionMessage(e), "\n"
                    ))
                e
            },
            .checkRequiredColumnsDF(.x, .y)
        )

    }) %>%
        purrr::discard(is.null) %>%
        invisible()

}

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
#' @return invisibly returns an error condition
#' (subclass "invalid_column_values" or "invalid_column_class"),
#' and it also prints an error message if the column contains invalid values.
#' If `quiet_success` is FALSE an no errors were found, it prints a message
#' indicating that no errors were found.
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

        template <- .template(dat)

        if (!col %in% template[["column_name"]])
            .stop_uncataloged_column(col, dat_name)

        type_of_test <- template[["test"]][template[["column_name"]] == col]

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
            fun_output <-
                paste0("^",
                       paste0( "(", eval(call(fun)), ")", collapse = "|"),
                       "$")
            values_lgl <- grepl(fun_output, col_values) | is.na(col_values)

            if (!all(values_lgl)) {

                invalid_values <- col_values[!values_lgl]
                n_rows <- length(invalid_values)
                invalid_pos <- seq_along(col_values)[!values_lgl]

                .stop_invalid_column_values(
                    col, n_rows, dat_name, invalid_values,
                    invalid_pos = invalid_pos
                )

            }

        } else if (type_of_test == "class") {

            col_class <- class(dat[[col]])
            class_opts <-
                template[["valid_values"]][ template[["column_name"]] == col]
            class_lgl <- grepl(class_opts, col_class)

            if (!class_lgl)
                .stop_invalid_column_class(col_class, dat_name)

        }

        if (!quiet_success) {

            if (!is.null(dat_name)) {

                message(crayon::green(
                    ">>> The column ", col, "of the ", dat_name,
                    "dataset contains valid values."
                ))

            } else {

                message(crayon::green(
                    ">>> The column ", col, "contains valid values."
                ))

            }

        }

}


#' Check column values in a data frame
#'
#' \code{.checkColumnValuesDF} applies the \code{\link{.checkColumnValues}}
#' function to all of the columns of a single dataset.
#'
#' @param dat A data frame.
#' @param dat_name Character string indicating the name of the dataset.
#' Default is NULL.
#'
#' @return Invisibly returns a list of error
#' conditions (subclass "invalid_column_values" or "invalid_column_class"),
#' and it also prints an error message if a column contains invalid values.
#'
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr discard
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

    col_names <- colnames(dat)

    purrr::map(col_names, ~{

        tryCatch(
            uncataloged_column = function(e) {
                message(crayon::red(conditionMessage(e), "\n"))
                e
            },
            invalid_column_values = function(e) {
                message(crayon::red(conditionMessage(e), "\n"))
                e
            },
            invalid_column_class = function(e) {
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
        purrr::discard(is.null) %>%
        invisible()

}

#' Check column values in a list of bugphyzz datasets
#'
#' \code{.checkColumnValuesList} applies the \code{.checkColumnValuesDF}
#' function to a list of bugphyzz datasets.
#'
#' @param list A list of bugphyzz datasets.
#'
#' @return Invisibly returns a list of error conditions (subclass
#' "invalid_column_values" or "invalid_column_class"), and it also prints
#' an error message if a column contains invalid values in any of the datasets
#' in the list.
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
.checkColumnValuesList <- function(dats) {

    dats_names <- names(dats)

    purrr::map2(dats, dats_names, ~ {.checkColumnValuesDF(.x, .y)}) %>%
        purrr::set_names(dats_names) %>%
        purrr::discard(is.null) %>%
        invisible()

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
#' \code{.stop_required_columns_missing} generates an error condition if one or
#' more of the required columns is missing from a bugphyzz dataset.
#' The required columns can be printed with the \code{\link{.requiredColumns}}
#' function. This function should be used within the
#' \code{\link{.checkRequiredColumnsDF}} function.
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
#' \code{\link{.stop_required_columns_misplaced}}
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

    .stop_custom(subclass = "required_columns_missing", message = msg, ...)

}

#' Stop condition for misplaced required columns in a bugphyzz dataset
#'
#' \code{.stop_required_columns_misplaced} generates an error condition if one
#' or more of the required columns is not in the right order in a bugphyzz
#' dataset. The order of the required columns can be printed with the
#' \code{\link{.requiredColumns}} function. This function should be used within
#' the \code{\link{.checkRequiredColumnsDF}} function.
#'
#' @param cols A character vector containing the names of the missing columns.
#' @param dataset_name A character string indicating the name of the dataset.
#' @param df A data frame with information about the expected and actual
#' positions of the required columns in a bugphyzz dataset. See
#' \code{\link{.checkRequiredColumnsDF}}.
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
#' \code{\link{.stop_required_columns_misplaced}}
#'
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

    .stop_custom(subclass = "required_columns_misplaced",
        message = msg, df = df, misplaced_cols = cols, ...)

}

#' Stop condition for invalid values in a column of a bugphyzz dataset
#'
#' \code{.stop_invalid_column_values} generates an error condition if a column
#' contains invalid values. This function should be used within the
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
#' \code{\link{.stop_uncataloged_column}}
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

            values <- paste0(
                utils::head(as.character(values), n = 3), collapse = "; "
            )

            msg <- paste0(msg, " The first invalid values are: ", values, "...")
        }

        .stop_custom(
            subclass = "invalid_column_values", message = msg, n_rows = n_rows,
            invalid_values = values, ...
        )

}

#' Stop condition for invalid column class (in Attribute_value column)
#'
#' \code{.stop_invalid_column_class} generates an error condition if a column
#' (Attribute_value) is not logical or numeric.
#'
#' @param col_class A character string containing the name of an invalid class.
#' @param dat_name A character string with the name of the dataset.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition.
#' Object of class: "invalid_column_class", "error", "condition".
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_invalid_column_values}};
#' \code{\link{.stop_invalid_column_class}};
#' \code{\link{.stop_uncataloged_column}}
#'
.stop_invalid_column_class <- function(col_class, dat_name = NULL, ...) {

    # Currently, this stop function is only for the "Attribute_value" column

    if (!is.null(dat_name)) {

        msg <- paste0(
            ">>> Invalid column class in ", dat_name, ".",
            " The `Attribute_value` column` should be of class logical or",
            " numeric, not ", col_class, "."
        )

    } else {

        msg <- paste0(
            ">>> Invalid column class.",
            " The `Attribute_value` column` should be of class logical or",
            " numeric, not ", col_class, "."
        )
    }

    .stop_custom(subclass = "invalid_column_class", message = msg, ...)

}

#' Stop condition for an un-cataloged column
#'
#' \code{.stop_uncataloged_column} returns an error when a column is not
#' cataloged in the extdata/template.csv file. This function shold be used
#' within the \code{\link{.checkColumnValues}} function.
#'
#' @param col Character string indicating the column name.
#' @param dat_name Character string inticating the dataset name.
#' @param ... Any other argument useful to identify the source of the error
#' and/or how to fix it.
#'
#' @return
#' Error condition.
#' Object of class: "invalid_column_class", "error", "condition".
#'
#' @keywords internal
#'
#' @family custom stop functions
#' @seealso
#' \code{\link{.stop_custom}};
#' \code{\link{.stop_invalid_column_values}};
#' \code{\link{.stop_invalid_column_class}};
#' \code{\link{.stop_uncataloged_column}}
#'
.stop_uncataloged_column <- function(col, dat_name = NULL, ...) {

    if (!is.null(dat_name)) {

        msg <- paste0(
            ">>> uncataloged column. The column ", col, " of the dataset ",
            dat_name, " cannot be checked because it's not included in the",
            " template file. Please add it to extdata/template.tsv"
        )

    } else {

        msg <- paste0(
            ">>> uncataloged column. The column ", col,
            " cannot be checked because it's not included in the template",
            " file. Please add it to extdata/template.tsv."
        )

    }

    .stop_custom(subclass = "uncataloged_column", message = msg, ...)

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
#' \code{\link{.requiredColumns}};
#' \code{\link{.checkRequiredColumnsDF}};
#' \code{\link{.checkRequiredColumnsDFList}}
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
    c(
        "NCBI_ID",
        "Genome_ID",
        "Accession_ID",
        "Taxon_name",
        "Attribute",
        "Attribute_value",
        "Attribute_source",
        "Evidence",
        "Confidence_interval"
    )
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
#' @family helper functions
#'
#' @importFrom utils read.table
#'
#' @seealso
#' \code{\link{.template}};
#' \code{\link{.attributes}};
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
#' @family helper functions
#' @seealso
#' \code{\link{.template}};
#' \code{\link{.attributes}};
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
