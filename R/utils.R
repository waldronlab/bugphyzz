#' Fill in NCBI IDs where the cells have NA
#'
#' \code{fill_NCBI} fills NA cells in the NCBI_ID column with taxids according
#' to the Taxon_name column.
#'
#' @param data A dataframe from the \link{physiologies} function.
#' @return A dataframe.
#' @keywords internal
#'
#' @importFrom dplyr rename
#' @importFrom rlang .data
#' @importFrom taxize get_ids
#' @importFrom mgsub mgsub
#' @examples
#' \dontrun{
#' aer <- physiologies("aerophilicity")[[1]]
#' aer_NCBI <- fill_NCBI(aer)
#' }
fill_NCBI <- function(data)
{
  null_df <- data[is.na(data$NCBI_ID),]
  ids <- taxize::get_ids(null_df[["Taxon_name"]], db = 'ncbi')
  ncbi <- ids[["ncbi"]]
  null_df$NCBI_ID <- ncbi

  df <- merge(data, null_df[,c("Taxon_name", "NCBI_ID")], by = "Taxon_name", all.x = T) %>%
    dplyr::rename(NCBI_ID = .data[["NCBI_ID.x"]])
  df$NCBI_ID <- paste(df$NCBI_ID, df$NCBI_ID.y)
  df$NCBI_ID <- mgsub::mgsub(df$NCBI_ID, c("NA", " "), c("", ""))
  df$NCBI_ID.y <- NULL
  df <- unique(df)
  return(df)
}

.DISCRETE_TYPES <- c(
    'multistate-intersection', 'multistate-union', 'binary'
)

