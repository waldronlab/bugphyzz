#' BugPhyzz's cache
#'
#' \code{getCache} starts or loads the local cache. The location of the path is
#' determined by \code{tools::R_user_dir(package = "curatedMetagenomicDataClient", which = "cache")}.
#'
#' @keywords internal
#'
.getCache <- function() {
  cache_path <- tools::R_user_dir(package = "bugphyzz", which = "cache")
  cache <- BiocFileCache::BiocFileCache(cache = cache_path, ask = FALSE)
  return(cache)
}

#' Create taxonomy table
#'
#' \code{.createTaxonomyTable} extracts all of the unique NCBI IDs across
#' BugPhyzz and uses the \code{\link[taxize]{classification}} function to
#' retrieve the full taxonomy ranks from the NCBI database, and then saves it as
#' a table in the user's cache with the \code{BiocFileCache} package. The
#' cache can be accessed with the \code{\link{.getCache}} function.
#'
#' Two versions of the taxonomy table are saved in a list object (.RData),
#' one version with taxonomy names and another with NCBI IDs. See the
#' \code{\link{taxonomyTable}} function to know how to access each of them.
#'
#' @return A taxonomy table (*.RData object) saved in the cache, and a message
#' indicating that the taxonomy table has been saved to the cache.
#'
#' @examples
#'
#' \dontrun{
#' library(bugphyzz)
#' .createTaxonomyTable()
#'
#' }
#'
.createTaxonomyTable <- function() {

  # define function to modify the output of taxize::classification
  # into a tibble.
  # A similar result can be achieved with cbind. Consider using cbind
  # instead of reshape_taxon_table
  reshape_taxon_table <- function(x, type) {
    if (tail(x[["rank"]], 1) == "no rank") {
      x$rank[length(x[["rank"]])] <- "no_rank"
    }
    x <- x[!x[["rank"]] %in% c("no rank", "clade"), ]
    tibble::as_tibble(t(x[type]), .name_repair = "minimal") %>%
      magrittr::set_colnames(gsub(" ", "_", x[["rank"]]))
  }

  suppressMessages({
    datasets <- physiologies()
    datasets[["fatty acid composition"]] <- fattyAcidComposition()
  })

  suppressWarnings({
    # There could be some warning of NAs introdcued by coercion when
    # converting to integer
    ncbi_ids <- datasets %>%
      # as.integer helps to remove invalid NCBI IDs
      purrr::map(~ as.integer(.x[["NCBI_ID"]])) %>%
      purrr::flatten_int() %>%
      unique() %>%
      .[!is.na(.)] %>%
      sort() %>%
      rev() # very large IDs might cause errors, is better to order by integer size

    # There could be some warnings about NAs because of invalid NCB IDs
    taxonomies <- suppressMessages(taxize::classification(ncbi_ids, db = "ncbi", batch_size = 1000))

  })

  # This step is necessary because invalid IDs might cause that outputs
  # become misplaced with respect to the vector names
  new_names <- map_chr(taxonomies, ~tail(.x[["id"]], 1))
  names(taxonomies) <- new_names

  ranks <- taxonomies %>%
    purrr::map_chr(~tail(.x$rank, 1))
  ranks_table <- tibble::tibble(NCBI_ID = names(ranks), rank = ranks)

  taxonomy_table_names <- taxonomies %>%
    purrr::map(~ reshape_taxon_table(.x, type = "name")) %>% # function defined above
    dplyr::bind_rows(.id = "NCBI_ID") %>%
    dplyr::distinct() # remove duplicates

  taxonomy_table_ids <- taxonomies %>%
    purrr::map(~ reshape_taxon_table(.x, type = "id")) %>% # function defined above
    dplyr::bind_rows(.id = "NCBI_ID") %>%
    dplyr::distinct() # remove duplicates

  taxonomy_table_with_ranks_names <- dplyr::left_join(ranks_table, taxonomy_table_names, by = "NCBI_ID")
  taxonomy_table_with_ranks_ids <- dplyr::left_join(ranks_table, taxonomy_table_ids, by = "NCBI_ID")

  taxonomy_tables <- list(name = taxonomy_table_with_ranks_names, id = taxonomy_table_with_ranks_ids)

  # The following steps are for saving to cache
  cache <- .getCache()
  cache_info <- BiocFileCache::bfcinfo(cache)
  rids <- cache_info %>%
    dplyr::filter(rname == "taxonomy_tables") %>%
    dplyr::pull(rid)
  if (length(rids) > 0) {
    for (i in seq_along(rids)) {
      BiocFileCache::bfcremove(cache, rids[i])
    }
  }
  save_path <- BiocFileCache::bfcnew(cache, "taxonomy_tables", ext=".RData")
  save(taxonomy_tables, file = save_path)

  message("A new taxonomy table has been saved to cache.")

}

#' Taxonomy table
#'
#' \code{taxonomyTable} imports a taxonomy table of all of the NCBI IDs
#' across BugPhyzz.
#'
#' When the function is used for the first time, a taxonomy table is created
#' with \code{bugpyzz:::.createTaxonomyTable} before being imported. If NCBI IDs
#' have been added or removed from the BugPhyzz database, the taxonomy table
#' can be updated by setting the `update` parameter to TRUE. This will create
#' a new taxonomy table with \code{bugpyzz:::.createTaxonomyTable}.
#'
#' Columns:
#'
#' + NCBI_ID: This column contains the original NCBI ID used to get the
#' taxonomy table. It can be used to join the taxonomy table with any bugphyzz
#' dataset.
#'
#' + rank: This column indicates the rank of the original NCBI_ID. Usually at
#' the genus, species, or strain level.
#'
#' + taxonomy ranks: There are other several columns such as superkingdom,
#' phylum, class, order, family, and genus filled with values for most NCBI IDs.
#' There are other columns such a species, strain, among a few others.
#'
#' @param type A character vector of length 1. Options: "name" and "id".
#' If "name" the output will contain taxa names. If "id" the output will
#' contain NCBI IDs.
#'
#' @param update If TRUE a new taxonomy table is created before importing.
#' If FALSE the taxonomy table currently stored in the user's cache is
#' imported.
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(bugphyzz)
#' taxonomy_table <- taxonomyTable(type = "name", update = FALSE)
#' taxonomy_table
#' }
#'
taxonomyTable <- function(type = "name", update = FALSE) {

  cache <- .getCache()

  rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_tables", field = "rname") %>%
    dplyr::pull(rid)

  if (!length(rid)) {
    cat("There is no taxonomyTable saved in cache. Creating one. This might take a few minutes.", sep = "\n")
    .createTaxonomyTable()
    rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_tables", field = "rname") %>%
      dplyr::pull(rid)
  } else if (isTRUE(update)) {
    cat("Update parameter was set to TRUE. Creating a new taxonomyTable. This might take a few minutes.", sep = "\n")
    .createTaxonomyTable()
    rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_tables", field = "rname") %>%
      dplyr::pull(rid)
  }

  rpath <- BiocFileCache::bfcrpath(cache, rids = rid)
  taxonomy_tables <- get(load(rpath))
  taxonomy_tables[[type]]
}
