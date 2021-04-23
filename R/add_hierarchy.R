utils:::globalVariables("NCBI_ID")

#' Get taxinomic rank names from an NCBI taxid
#'
#' This function is a wrapper around \code{taxize::classification}. It takes a NCBI taxid and a taxonomic rank and returns the name of the taxa at the specified taxonomic rank. Valid inputs for taxonomic rank (for Bacteria): "superkingdom", "phylum", "class", "order", "family", "genus", "species."
#'
#' @param x A double, integer, or character indicating a valid NCBI taxid.
#' @param y A string indicating the taxonomic rank, e.g. "phylum."
#' @return A string with the name of the taxa.
#' @importFrom taxize classification
#' @family taxonomy functions
#' @seealso
#'  \code{\link{add_hierarchy}};
#'  \code{\link[taxize]{classification}}
#' @export
#'
#' @examples
#'
#' get_hierarchy(906, "phylum")
#' get_hierarchy(906, "genus")
get_hierarchy <- function(x, y) {
    resp <- taxize::classification(x, db = "ncbi")
    output <- resp[[1]][resp[[1]]$rank == y, ][["name"]]
    if (!length(output)) {
        return(NA)
    } else {
        return(output)
    }
}

#' Add taxonomic rank names
#'
#' Add a column containing taxonomic rank names to a data frame.
#'
#' @param df A data frame with a column named "NCBI_ID" containing taxids.
#' @param y A string indicating the required taxonomic rank. e.g. "phylum."
#'
#' @return Data frame with an additional column containing taxa names.
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom dplyr relocate
#' @importFrom rlang :=
#' @family taxonomy functions
#' @seealso
#'  \code{\link{get_hierarchy}};
#'  \code{\link[taxize]{classification}}
#'
#' @export
#'
add_hierarchy <- function(df, y) {

    y_var <- rlang::enquo(y)
    output <- df %>%
        dplyr::mutate(
            !!y_var := purrr::map_chr(NCBI_ID, ~get_hierarchy(.x, y))
        ) %>%
        dplyr::relocate(!!y_var)
    return(output)
}

