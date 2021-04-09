utils::globalVariables(c("Attribute_value", "value"))

#' Add BugPhyzz attribute values as columns to a data frame
#'
#' \code{add_attributes} adds columns to a data frame corresponding to attribute values from a BugPhyzz database.
#'
#' The **data frame** must have a column named "NCBI_ID" with NCBI taxids and a column named "Abundance" with relative abundance values.
#'
#' The **database** (downloaded from BugPhyzz) must have a column named "NCBI_ID" with NCBI taxids and a column named "Attribute_value."
#'
#' NAs are converted to 0s.
#'
#' @param data A data frame containing abundance values.
#' @param database A data frame containing BugPhyzz attribute values.
#'
#' @return A data frame with new columns corresponding to BugPhyzz attribute values.
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_squish
#' @importFrom magrittr %>%
#' @family signature score functions
#' @seealso
#' \code{\link{get_score}}
#'
#' @export
#'
#' @examples
#' # Add cellular respiration attributes
#' df <- AsnicarF2017_genus[[1]]
#' db <- attribute(keyword = "aerophilicity")[[1]]
#'
#' x <- add_attributes(df, db)
#' x
#'
add_attributes <- function(data, database) {

    db <- database %>%
        dplyr::mutate(
            Attribute_value = stringr::str_to_lower(Attribute_value) %>%
                stringr::str_squish()
        )

    attr_values <- unique(db[["Attribute_value"]])

    for ( attr in attr_values) {

        taxids <- db[db[["Attribute_value"]] == attr, ][["NCBI_ID"]]
        output <- vector("double", length(data[["NCBI_ID"]]))

        for (i in seq_along(output)) {

            if (data[["NCBI_ID"]][[i]] %in% taxids) {

                output[[i]] <- data[i, "Abundance"][[1]]

            } else {

                output[[i]] <- 0

            }
        }

        data[[attr]] <- output

    }

    return(data)

}


# The function below only adds TRUE FALSE values.
#
# add_attributes2 <- function(data, database) {
#
#     db <- database %>%
#         dplyr::mutate(
#             Attribute_value = stringr::str_to_lower(Attribute_value)
#         )
#
#     attr_values <- unique(db$Attribute_value)
#
#     for (i in attr_values) {
#
#         taxids <- db[db$Attribute_value == i, ]$NCBI_ID
#         data[[i]] <- data$NCBI_ID %in% taxids
#
#     }
#
#     return(data)
#
# }
#
#





