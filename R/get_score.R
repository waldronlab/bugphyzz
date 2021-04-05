utils::globalVariables(c("Attribute", "value"))

#' Get signature scores from an abundance/attributes matrix
#'
#' \code{get_score} calculates a signature score for each attribute value (one attribute per column) by adding up the relative abundance values. Since the abundance values are added up by column, the values are always between 0 and 100.
#'
#' @param x A data frame. Output from the  \code{add_attributes} function.
#'
#' @return A data frame of two columns: "Attributes" and "Score."
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom dplyr count
#' @family signature score functions
#' @seealso
#' \code{\link{add_attributes}}
#' @export
#'
#' @examples
#' df <- AsnicarF2017_genus[[1]]
#' db <- attribute(keyword = "oxygen")[[1]]
#'
#' x <- add_attributes(df, db)
#' x
#'
#' y <- get_score(x)
#' y
get_score <- function(x) {
    df <- x[, -c(1,2)] %>%
        tidyr::pivot_longer(names_to = "Attribute", tidyselect::everything()) %>%
        dplyr::count(Attribute, wt = value, name = "Score")
    return(df)
}

