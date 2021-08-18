#' Fetch signatures for easy GMT file creation
#'
#' @param df A `data.frame` output by the \link{physiologies} function
#' @return A list of attribute vectors
#' @export
#'
#' @examples
#' bugphyzz_GMT <- lapply(phys, get_signatures) %>%
#' unlist(recursive = F)
#' EnrichmentBrowser::writeGMT(bugphyzz_GMT, gmt.file = "data/bug_physiologies.gmt")
gmtSignatures <- function(df)
{
  temp <- split(df, df$Attribute)
  temp <- lapply(temp, function(x) subset(x, select = Taxon_name))
  temp <- lapply(temp, dplyr::pull, name = Taxon_name)
}
