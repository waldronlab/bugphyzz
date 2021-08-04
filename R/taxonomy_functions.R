#' Get Taxonomy Ranks from NCBI Taxonomy IDs
#'
#' \code{ncbiRank} retrieves taxonomy rank names (or IDs) at the kingdom, phylum, class,
#' order, family, genus, and species level for given NCBI taxonomy IDs.
#'
#' @param x
#' A numeric vector or character vector of valid NCBI taxonomy IDs.
#'
#' @param taxid
#' A logical vector of length 1. If TRUE, taxonomy IDs (taxids) are returned instead of rank names.
#' Default is FALSE.
#'
#' @return
#' A tibble. The first column, NCB_ID, contains the input taxonomy IDs.
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom taxize classification
#'
#' @examples
#' library(bugphyzz)
#' taxonomy_ids <- c(1578, 745, 562, 2)
#'
#' # Example 1
#' rank_names <- ncbiRank(taxonomy_ids)
#' rank_names
#'
#' # Example 2
#' rank_ids <- ncbiRank(taxonomy_ids, taxid = TRUE)
#' rank_ids
#'
ncbiRank <- function(x, taxid = FALSE, ranks = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species")) {

  ncbi_results <- suppressMessages(taxize::classification(x, db = "ncbi"))

  output <- vector("list", length(ranks) + 1) # ranks + 1 because of NCBI_ID column
  names(output) <- c("NCBI_ID", ranks)
  output[[1]] <- as.integer(names(ncbi_results)) # Column of NCBI_IDs

  for (i in seq_along(ranks)) {
      output[[i + 1]] <- purrr::map_chr(ncbi_results, function(x) {

        if (taxid) {
          rank_name <- x$id[ x$rank == ranks[i] ]
        } else{
          rank_name <- x$name[ x$rank == ranks[i] ]
        }

        ifelse(!length(rank_name), NA, rank_name)})
  }

  output <- tibble::as_tibble(output)
  return(output)

}
