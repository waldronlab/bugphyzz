utils::globalVariables(c("Sample", "Abundance"))

#' Get summarized abundance values per attribute
#'
#' \code{get_abundance_attributes} takes a data frame in which the first column
#' contains NCBI taxids and the remaining columns contain abundance values
#' (one column per sample).
#'
#' @param data A data frame where the first column is named "NCBI_ID" and the
#' remaining columns contain abundance values per sample.
#' @param database A data set from the \code{bugphyzz} package, e.g. aerophilicity.
#'
#' @return A data frame (tibble) containing the summarized abundance values per
#' bugphyzz attribute per sample.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect contains
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(bugphyzz)
#'
#' aerophilicity <- physiologies(keyword = "aerophilicity")[[1]]
#'
#' fpath <- system.file(path = "extdata", file = "AsnicarF_2017_genus.tsv", package = "bugphyzz")
#' asnicarf2017 <- readr::read_tsv(fpath)
#'
#' attributes_abundance <- get_attributes_abundance(asnicarf2017, aerophilicity)
#' attributes_abundance
#'
get_attributes_abundance <- function(data, database) {

  data$NCBI_ID <- as.character(data$NCBI_ID)
  database$NCBI_ID <- as.character(database$NCBI_ID)

  x <- data %>%
    tidyr::pivot_longer(cols = !tidyselect::contains("NCBI_ID"), names_to = "Sample",
                        values_to = "Abundance")

  x <- dplyr::left_join(x, database[, c("NCBI_ID", "Attribute")],
                        by = "NCBI_ID") %>%
    dplyr::filter(!is.na(Attribute), ) %>%
    dplyr::distinct() %>%
    dplyr::select(-NCBI_ID) %>%
    dplyr::group_by(Sample, Attribute) %>%
    dplyr::summarise(Abundance = sum(Abundance, na.rm = TRUE)) %>%
    dplyr::ungroup()

  x <- tidyr::pivot_wider(x, names_from = Sample, values_from = Abundance,
                          values_fill = 0)

  return(x)

}
