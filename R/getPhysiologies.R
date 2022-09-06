
#' Get physiologies
#'
#' \code{getPhysiologies} get all physiology annotations for one or more taxa.
#'
#' @param x A valid NCBI ID or taxon name
#' @param phys A list of dataframes with physiologies imported with the
#' `physiologies` function. If not specified, all physiologies will be imported.
#' @param taxid.type The type of taxid. Valid options: Taxon_name or NCBI_ID.
#' Default is Taxon_name.
#'
#' @return A list of physiologies per taxa.
#' @export
#'
#' @examples
#'
#' phys <- physiologies()
#'
#' taxa_names <- c('Escherichia coli', 'Staphylococcus aureus')
#' taxa_ids <- c('562', '1280')
#'
#' phys_names <- getPhysiologies(taxa_names, phys)
#' phys_ids <- getPhysiologies(taxa_ids, phys, 'NCBI_ID')
#'
getPhysiologies <- function(x, phys = physiologies(), taxid.type = 'Taxon_name') {

  if (!taxid.type %in% c('Taxon_name', 'NCBI_ID')) {
    stop('Only Taxon_name and NCBI_ID values are valid.', call. = FALSE)
  }

  col <- taxid.type

  output <- vector('list', length(x))
  names(output) <- x
  for (i in seq_along(output)) {
    output[[i]] <- purrr::map(phys, ~ .x[which(.x[[col]] == x[[i]]), ]) |>
      purrr::discard(~ !nrow(.x)) |>
      purrr::map( ~{
        if (is.logical(.x$Attribute_value)) {
          .x <- .x[which(.x$Attribute_value == TRUE),]
          return(.x$Attribute)
        } else if (is.numeric(.x$Attribute_value)) {
          return(.x$Attribute_value)
        }
      }) |>
      purrr::discard(~ !length(.x))
  }
  return(output)
}
