utils::globalVariables(c("functionname", "Br-C10:1", "Oxo-C19:1"))
#' Function for shaping fatty acid composition from Google Sheets using tidyR
#'
#' @return a tidy data frame of fattyAcidComposition from customlinks.tsv
#'
#' @importFrom  tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(bugphyzz)
#' fattyAcidComposition()
#' x <- fattyAcidComposition()
#' dplyr::glimpse(x)
#'
#load fatty acid composition sheet from customlinks.tsv
#test to check whether custom link exists
fattyAcidComposition <- function(){
  link <- customLinks() %>%
    dplyr::filter(functionname == "fattyAcidComposition") %>%
    dplyr::pull(link)
  fac_wide <- read.csv(link,check.names = FALSE)
  fac_long <- fac_wide %>%
    pivot_longer(cols = `Br-C10:1`:`Oxo-C19:1`,
                   names_to = "new_attribute",
                   values_to = "new_attribute_value")
  fac_long <- merge(x = fac_long, y = ranks_parents, by = "NCBI_ID", all.x = TRUE)

  return(fac_long)
}

#' Custom Links
#'
#' \code{customLinks} returns a data frame with links to custom data sets not included
#' in the \code{physiologies} function.
#'
#' @param keyword a character vector of custom links desired.
#' Use "all" for all available custom links.
#'
#' @return
#' A data frame with custom links.
#'
#' @export
#'
#' @examples
#' library(bugphyzz)
#' customLinks(keyword = "all")
customLinks <- function(keyword = "all"){
  fname <-
    system.file("extdata/customlinks.tsv", package = "bugphyzz")
  links <- read.table(fname, header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}
