#' Fetch physiological data
#'
#' @param keyword a character vector of physiologies desired (see \code{\link{curationLinks}}). For the available
#' physiologies, run bugphyzz::physiologies_list()
#'
#' @return a large list of tidy data.frames
#' @export
#'
#' @examples
#' x <- physiologies()
#' head(x[[1]])
#'
#' y <- physiologies(c("gram", "aerophilicity"))
#' head(y[[1]])
physiologies <- function(keyword = "all"){
  links <- curationLinks(keyword = keyword)

  ifelse(keyword[1] == "all", links, links <- links[links$physiology %in% keyword,])

  sheets <- as.list(links[[2]])
  dat <- lapply(sheets, read.csv)
  names(dat) <- links[[1]]
  dat
}

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
#' curationLinks(keyword = c("aerophilicity", "gram"))
curationLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "links.tsv"), package = "bugphyzz")
  links <- read.table(fname, sep = "\t")
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}