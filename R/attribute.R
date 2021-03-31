#' Fetch physiological attribute data
#'
#' @param keyword a character vector of attribute names desired. For the available
#' attributes, run bugphyzz::attribute_list()
#'
#' @return a large list of tidy data.frames
#' @export
#'
#' @examples
#' x <- attribute()
#' head(x[[1]])
#'
#' y <- attribute(c("gram", "oxygen"))
#' head(y[[1]])
attribute <- function(keyword = "all"){
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTDk8wow4yN_IrqbltZP-6w4rwf0JCNRiPL9jWWSqvTu4da6AgxJNAED98r-rSJeFE1msqsBpzPlk4a/pub?output=csv"
  links <- read.csv(url)

  ifelse(keyword[1] == "all", links, links <- links[links$Physiology %in% keyword,])

  sheets <- as.list(links[[2]])
  dat <- lapply(sheets, read.csv)
  names(dat) <- links[[1]]
  dat
}
