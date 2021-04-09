#' Fetch physiological attribute data
#'
#' @param keyword a character vector of attribute names desired (see \code{\link{curationLinks}}). For the available
#' attributes, run bugphyzz::attribute_list()
#'
#' @return a large list of tidy data.frames
#' @export
#'
#' @examples
#' x <- attribute()
#' head(x[[1]])
#'
#' y <- attribute(c("gram", "aerophilicity"))
#' head(y[[1]])
attribute <- function(keyword = "all"){
  links <- curationLinks(keyword = keyword)

  ifelse(keyword[1] == "all", links, links <- links[links$Physiology %in% keyword,])

  sheets <- as.list(links[[2]])
  dat <- lapply(sheets, read.csv)
  names(dat) <- links[[1]]
  dat
}

#' Show links to curation spreadsheets
#'
#' @param keyword a character vector of attribute names desired. For the available
#' attributes, run bugphyzz::attribute_list(). Use "all" for all available attributes.
#'
#' @return a data.frame with attribute names and URLs
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
           links[links$Physiology %in% keyword,])
  return(links)
}
