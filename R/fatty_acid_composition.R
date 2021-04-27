#function for shaping fatty acid composition from Google Sheets using tidyR

#' @param keyword a character vector of custom links desired. For the available
#' custom links, run bugphyzz::custom_link(). Use "all" for all available custom links.
#'
#' @return a tidy data frame of fattyacidComposition from customlinks.tsv
#'
#' @export
#'
#' @examples
#'
#'
#' customLinks()
#' customLinks(keyword = "fatty_acid_composition")


library(bugphyzz)
library(tidyr)

#load fatty acid composition sheet from customlinks.tsv
#test to check whether custom link exists

fatty_acid_composition <- function(keyword = "all"){
  customLinks()[["link"]]

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

  fac_long <- fac_wide %>%
    pivot_longer(cols = -"Br-C10:1",	"Br-C11:1",	"Br-C12:1",	"Br-C13:1",	"Br-C13:0",	"Br-C14:1",	"Br-C14:0",	"Br-C15:1",	"Br-C15:0",	"Br-C15:1",	"Br-C16:1",	"Br-C16:0",	"Br-C17:1",	"Br-C17:0",	"Br-C17:1",	"Br-C18:1",	"Br-C18:0",	"Br-C19:1",	"Br-C19:0",	"Br-C20:1",	"Br-C20:0",	"C10:1",	"C10:0",	"C11:1",	"C11:0",	"C12:1",	"C12:0",	"C12:2",	"C13:1",	"C13:0",	"C14:1",	"C14:0",	"C14:1",	"C15:1",	"C15:0",	"C16:1",	"C16:0",	"C16:1",	"C16:2",	"C16:3",	"C17:1",	"C17:0",	"C17:1",	"C17:2",	"C18:1",	"C18:0",	"C18:1",	"C18:2",	"C18:3",	"C18:4",	"C19:1",	"C19:0",	"C20:1",	"C20:0",	"C20:1",	"C20:2",	"C20:3",	"C20:4",	"C20:5",	"C21:1",	"C22:1",	"C22:0",	"C22:1",	"C22:2",	"C22:3",	"C22:4",	"C22:5",	"C23:1",	"C23:0",	"C24:1",	"C24:0",	"C25:1",	"C26:1",	"C26:0",	"C28:1", "C30:1", "C6:1",	"C6:0",	"C8:1",	"C8:0",	"C9:1",	"C9:0",	"Cyc-C13:1",	"Cyc-C15:1",	"Cyc-C16:1",	"Cyc-C17:1",	"Cyc-C18:1",	"Cyc-C18:0",	"Cyc-C19:1",	"Cyc-C20:1",	"Cyc-C21:1",	"Cyc-C23:1",	"Cyc-C24:1",	"OH-Br-C11:1",	"OH-Br-C12:1",	"OH-Br-C13:1",	"OH-Br-C14:1",	"OH-Br-C15:1",	"OH-Br-C15:0",	"OH-Br-C16:1",	"OH-Br-C17:1",	"OH-Br-C18:1",	"OH-C10:1",	"OH-C11:1",	"OH-C12:1",	"OH-C12:0",	"OH-C13:1",	"OH-C14:1",	"OH-C14:0",	"OH-C15:1",	"OH-C15:0",	"OH-C16:1",	"OH-C16:0",	"OH-C17:1",	"OH-C17:0",	"OH-C18:1",	"OH-C18:0",	"OH-C24:1",	"OH-C26:1",	"OH-C28:1",	"OH-C28:0",	"OH-C30:1",	"OH-C30:0",	"OH-C32:0",	"OH-Cyc-C19:1",	"Other",	"Oxo-C18:1",	"Oxo-C19:1")
                 names_to = "Attribute_value"     # name is a string!
                 values_to = "Br-C10:1",	"Br-C11:1",	"Br-C12:1",	"Br-C13:1",	"Br-C13:0",	"Br-C14:1",	"Br-C14:0",	"Br-C15:1",	"Br-C15:0",	"Br-C15:1",	"Br-C16:1",	"Br-C16:0",	"Br-C17:1",	"Br-C17:0",	"Br-C17:1",	"Br-C18:1",	"Br-C18:0",	"Br-C19:1",	"Br-C19:0",	"Br-C20:1",	"Br-C20:0",	"C10:1",	"C10:0",	"C11:1",	"C11:0",	"C12:1",	"C12:0",	"C12:2",	"C13:1",	"C13:0",	"C14:1",	"C14:0",	"C14:1",	"C15:1",	"C15:0",	"C16:1",	"C16:0",	"C16:1",	"C16:2",	"C16:3",	"C17:1",	"C17:0",	"C17:1",	"C17:2",	"C18:1",	"C18:0",	"C18:1",	"C18:2",	"C18:3",	"C18:4",	"C19:1",	"C19:0",	"C20:1",	"C20:0",	"C20:1",	"C20:2",	"C20:3",	"C20:4",	"C20:5",	"C21:1",	"C22:1",	"C22:0",	"C22:1",	"C22:2",	"C22:3",	"C22:4",	"C22:5",	"C23:1",	"C23:0",	"C24:1",	"C24:0",	"C25:1",	"C26:1",	"C26:0",	"C28:1", "C30:1", "C6:1",	"C6:0",	"C8:1",	"C8:0",	"C9:1",	"C9:0",	"Cyc-C13:1",	"Cyc-C15:1",	"Cyc-C16:1",	"Cyc-C17:1",	"Cyc-C18:1",	"Cyc-C18:0",	"Cyc-C19:1",	"Cyc-C20:1",	"Cyc-C21:1",	"Cyc-C23:1",	"Cyc-C24:1",	"OH-Br-C11:1",	"OH-Br-C12:1",	"OH-Br-C13:1",	"OH-Br-C14:1",	"OH-Br-C15:1",	"OH-Br-C15:0",	"OH-Br-C16:1",	"OH-Br-C17:1",	"OH-Br-C18:1",	"OH-C10:1",	"OH-C11:1",	"OH-C12:1",	"OH-C12:0",	"OH-C13:1",	"OH-C14:1",	"OH-C14:0",	"OH-C15:1",	"OH-C15:0",	"OH-C16:1",	"OH-C16:0",	"OH-C17:1",	"OH-C17:0",	"OH-C18:1",	"OH-C18:0",	"OH-C24:1",	"OH-C26:1",	"OH-C28:1",	"OH-C28:0",	"OH-C30:1",	"OH-C30:0",	"OH-C32:0",	"OH-Cyc-C19:1",	"Other",	"Oxo-C18:1",	"Oxo-C19:1"

  fac_long
  read.csv(fac_long)

  #I also want to include column with the numeric values next to Attribute_value .

}

customLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "customlinks"), package = "bugphyzz")
  links <- read.table(fname, sep = "\t", header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}


