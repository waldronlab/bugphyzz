#function for shaping fatty acid composition from Google Sheets using tidyR

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

    fac_long <- fac_wide %>%
      pivot_longer(cols = `Br-C10:1`:`Oxo-C19:1`,
                   names_to = "new_attribute",
                   values_to = "new_attribute_value")
  }
  return(database)

  fac_long
  write.csv(fac_long)

  #I also want to include column with the numeric values next to Attribute_value .

}

customLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "customlinks.tsv"), package = "bugphyzz")
  links <- read.table(fname, header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}


