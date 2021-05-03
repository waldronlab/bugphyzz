#function for shaping fatty acid composition from Google Sheets using tidyR

#' @param keyword a character vector of custom links desired. For the available
#' custom links, run bugphyzz::custom_link(). Use "all" for all available custom links.
#'
#' @return a tidy data frame of fattyacidComposition from customlinks.tsv
#'
#' @import tidyr pivot_longer
#'
#' @export
#'
#' @examples
#' fattyAcidComposition()
#' x <- fattyAcidComposition()
#' View(x)
#'

#load fatty acid composition sheet from customlinks.tsv
#test to check whether custom link exists

fattyAcidComposition <- function(){
  fac_wide <- read.csv(customLinks()[["link"]],check.names = FALSE)
  fac_long <- fac_wide %>%
    pivot_longer(cols = `Br-C10:1`:`Oxo-C19:1`,
                   names_to = "new_attribute",
                   values_to = "new_attribute_value")

  return(fac_long)
}

  #I also want to include column with the numeric values next to Attribute_value .

customLinks <- function(keyword = "all"){
  fname <-
    system.file("extdata/customlinks.tsv", package = "bugphyzz")
  links <- read.table(fname, header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}


