#' Fetch Bergeys abstracts
#' This function fetches Kelly Eckenrode's and Jonathan Ye's scraping of the Bergey's abstracts and returns a tidy data.frame
#' @return A tidy data.frame
#' @export
#'
#' @examples
#' x <- bergeys_abstracts()
#' dim(x)
bergeys_abstracts <- function(){
  dat <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRfDmQEmkl0QxwQ8A-DfrPmrRu6jQRwMfkz3eBOjJQsvKOTfXD-nkcmF6kIhFWjI1i92xWeraZghK1F/pub?gid=1045368624&single=true&output=csv",
                  check.names = FALSE, stringsAsFactors = FALSE)
  return(dat)
}
