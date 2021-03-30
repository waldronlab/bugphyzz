#' Fetch list of available physiological attribute names
#'
#' @return a character vector of physiological attribute names
#' @export
#'
#' @examples
#' x <- attribute_list()
attribute_list <- function(){
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTDk8wow4yN_IrqbltZP-6w4rwf0JCNRiPL9jWWSqvTu4da6AgxJNAED98r-rSJeFE1msqsBpzPlk4a/pub?output=csv"
  dat <- read.csv(url)
  dat[[1]]
}
