#' Get Taxonomy Ranks from NCBI TaxIDs
#'
#' \code{get_taxonomy_ranks} retrieves taxonomy names for input NCBI taxids at
#' different taxonomy ranks.
#'
#' @param x
#' A numeric vector of valid NCBI taxids.
#'
#' @return
#' A tibble in which each row contains the input NCBI IDs and each column contains
#' the taxonomy names of the following ranks: kingdom, phylum, class, order,
#' family, genus, species.
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom taxize classification
#'
#' @examples
#' library(bugphyzz)
#' taxids <- c(1578, 745, 562, 2)
#' rank_names <- get_taxonomy_ranks(taxids)
#'
#'
get_taxonomy_ranks <- function(x) {

  ncbi_results <- suppressMessages(taxize::classification(x, db = "ncbi"))
  ranks<- c("superkingdom", "phylum", "class", "order", "family", "genus", "species")
  output <- vector("list", 8)
  names(output) <- c("NCBI_ID", ranks)
  output[[1]] <- names(ncbi_results)

  for (i in seq_along(ranks)) {
      output[[i + 1]] <- purrr::map_chr(ncbi_results, function(x) {
        rank_name <- x$name[x$rank == ranks[[i]]]
        names(rank_name) <- NULL
        ifelse(!length(rank_name), NA, rank_name)})
  }

  output <- tibble::as_tibble(output)
  output[["NCBI_ID"]] <- as.integer(output[["NCBI_ID"]])
  names(output)[2] <- "kingdom"
  return(output)

}

