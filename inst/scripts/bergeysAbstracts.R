#' Fetch Bergeys abstracts
#'
#' \code{.bergeyAbstracts} fetches Kelly Eckenrode's and Jonathan Ye's scraping
#' of the Bergey's abstracts and returns a tidy data.frame.
#'
#' @return A tidy data.frame.
#'
#' @keywords internal
#'
#' @examples
#' bergeys <- bugphyzz:::.bergeysAbstracts()
#' head(bergeys)
.bergeysAbstracts <- function(){
  fname <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRfDmQEmkl0QxwQ8A-DfrPmrRu6jQRwMfkz3eBOjJQsvKOTfXD-nkcmF6kIhFWjI1i92xWeraZghK1F/pub?gid=1045368624&single=true&output=csv"
  dat <- utils::read.csv(fname, check.names = FALSE, stringsAsFactors = FALSE)
  return(dat)
}
