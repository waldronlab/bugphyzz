#function for shaping fatty acid composition from Google Sheets using tidyR

library(bugphyzz)
library(tidyr)

#load fatty acid composition sheet from customlinks.tsv
#test to check whether custom link exists

fatty_acid_composition <- function(keyword = "all"){
  customlinks.tsv()[["link"]]

  if (all(keyword != "all") & !all(keyword %in% physiologies_list())) {
    stop("I don't recognize one or more of the provided custom links ",
         "Check valid physiologies with physiologies_list()")
  }
  links <- customLinks(keyword = keyword)[, ("link")]
  database <- vector("list", nrow(links))
  for (i in seq_along(database)) {
    names(database)[i] <- links[i, "physiology"]
    database[[i]] <- tidyr_function(read.csv(links[i, "link"]))
  }
  return(database)
}

#' Show links to custom link spreadsheet
#'
#' @param keyword a character vector of custom links desired. For the available
#' custom links, run bugphyzz::custom_link(). Use "all" for all available custom links.
#'
#' @return a data.frame if fattyacidComposition from customlinks.tsv
#' @export
#'
#' @examples
#' customLinks()
#' customLinks(keyword = "fatty_acid_composition")

customLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "customlinks.tsv"), package = "bugphyzz")
  links <- read.table(fname, sep = "\t", header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}

