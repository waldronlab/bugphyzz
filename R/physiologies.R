#' Fetch physiological data
#'
#' @param keyword a character vector of physiologies desired (see \code{\link{curationLinks}}). For the available
#' physiologies, run bugphyzz::physiologies_list()
#'
#' @return a large list of tidy data.frames
#' @importFrom utils read.table
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' x <- physiologies()
#' head(x[[1]])
#'
#' y <- physiologies(c("gram stain", "aerophilicity"))
#' head(y[[1]])
physiologies <- function(keyword = "all") {

  if (all(keyword != "all") & !all(keyword %in% physiologies_list())) {
    stop("I don't recognize one or more of the provided physiologies ",
         "Check valid physiologies with physiologies_list()")
  }
  links <- curationLinks(keyword = keyword)[, c("physiology", "link")]
  database <- vector("list", nrow(links))
  for (i in seq_along(database)) {
    names(database)[i] <- links[i, "physiology"]
    database[[i]] <- utils::read.csv(links[i, "link"])
    nmissing <- sum(is.na(database[[i]]$Attribute_value))
    if(nmissing > 0){
      message("Dropped ", nmissing, " rows with missing Attribute_value from ", names(database)[[i]])
    }else{
      message("Finished ", names(database)[[i]])
    }
    database[[i]] <- database[[i]][!is.na(database[[i]]$Attribute_value), ]
  }
  return(database)
}

# physiologies <- function(keyword = "all"){
#   links <- curationLinks(keyword = keyword)
#
#   ifelse(keyword[1] == "all", links, links <- links[links$physiology %in% keyword,])
#
#   sheets <- as.list(links[[2]])
#   dat <- lapply(sheets, read.csv)
#   names(dat) <- links[[1]]
#   dat
# }

#' Show links to curation spreadsheets
#'
#' @param keyword a character vector of physiologies desired. For the available
#' physiologies, run bugphyzz::physiologies_list(). Use "all" for all available physiologies.
#'
#' @return a data.frame with physiology names and URLs
#' @export
#'
#' @examples
#' curationLinks()
#' curationLinks(keyword = "aerophilicity")
#' curationLinks(keyword = c("aerophilicity", "gram stain"))
curationLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "links.tsv"), package = "bugphyzz")
  links <- utils::read.table(fname, sep = "\t", header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}
