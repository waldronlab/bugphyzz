#pacman::p_load("dplyr", "bugphyzz", "taxize", "ggplot2", "tidyr", "mgsub")
#' Fill in NCBI IDs where the cells have NA
#'
#' @param data a dataframe from the \link{physiologies} function
#' @return a dataframe
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' aer <- physiologies("aerophilicity")
#' aer_NCBI <- fill_NCBI(aer)
#' }
fill_NCBI <- function(data)
{
  null_df <- data[is.na(data$NCBI_ID),]
  ids <- get_ids(null_df[["Taxon_name"]], db = 'ncbi')
  ncbi <- ids[["ncbi"]]
  null_df$NCBI_ID <- ncbi

  df <- merge(data, null_df[,c("Taxon_name", "NCBI_ID")], by = "Taxon_name", all.x = T) %>%
    rename(NCBI_ID = NCBI_ID.x)
  df$NCBI_ID <- paste(df$NCBI_ID, df$NCBI_ID.y)
  df$NCBI_ID <- mgsub::mgsub(df$NCBI_ID, c("NA", " "), c("", ""))
  df$NCBI_ID.y <- NULL
  df <- unique(df)
  return(df)
}
