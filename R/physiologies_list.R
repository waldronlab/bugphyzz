#' Fetch list of available physiological physiologies names
#'
#' @return a character vector of physiological physiologies names
#' @export
#'
#' @examples
#' x <- physiologies_list()
physiologies_list <- function(){
  fname <-
    system.file(file.path("extdata", "links.tsv"), package = "bugphyzz")
  dat <- read.table(fname, sep = "\t", header = TRUE)
  return(dat$physiology)
}
