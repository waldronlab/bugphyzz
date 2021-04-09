#' Fetch list of available physiologies
#'
#' @return a character vector of physiologies
#' @export
#'
#' @examples
#' x <- physiologies_list()
physiologies_list <- function(){
  curationLinks()[["physiology"]]
}
