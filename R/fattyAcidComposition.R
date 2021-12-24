#' Import the fatty acid composition dataset
#'
#' \code{fattyAcidComposition} returns a data frame with data about the
#' fatty acid composition of microbes.
#'
#' @return A tidy data frame of fattyAcidComposition from customlinks.tsv
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' fattyAcidComposition()
#' x <- fattyAcidComposition()
#' x
#'
fattyAcidComposition <- function(){
    link <- customLinks() %>%
        dplyr::filter(.data[["functionname"]] == "fattyAcidComposition") %>%
        dplyr::pull(.data[["link"]])
    fac_wide <- utils::read.csv(link,check.names = FALSE)
    fac_long <- fac_wide %>%
        tidyr::pivot_longer(cols = .data[["Br-C10:1"]]:.data[["Oxo-C19:1"]],
                                      names_to = "Attribute_new",
                                      values_to = "Attribute_value")
    dplyr::left_join(fac_long, ranks_parents, by = "NCBI_ID") %>%
      as.data.frame()
}

#' Custom links
#'
#' \code{customLinks} returns a data frame with links to custom data sets not
#' not included in the \code{\link{physiologies}} function.
#'
#' @param keyword A character vector with the name of custom datasets.
#' Use "all" for all available custom links.
#'
#' @return A data frame with custom links.
#'
#' @importFrom utils read.table
#' @keywords internal
#'
#' @examples
#' bugphyzz:::customLinks(keyword = "all")
customLinks <- function(keyword = "all"){
    fname <-
        system.file("extdata/customlinks.tsv", package = "bugphyzz")
    links <- utils::read.table(fname, header = TRUE)
    ifelse(keyword[1] == "all", links, links <-
                      links[links$physiology %in% keyword,])
    links
}
