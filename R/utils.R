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
#'
#' @return NULL. A message is displayed indicating that the taxonomy table has
#' been saved to the cache.
#'
#' @keywords internal
#'
.createTaxonomyTable <- function() {

  valid_ranks = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species", "strain")
  tax_regex <- valid_ranks %>%
    c("query") %>%
    paste0(., collapse = "|") %>%
    paste0("^(", ., ")(_id)*$")

  suppressMessages({
    datasets <- physiologies("all")
    datasets[["fatty acid composition"]] <- fattyAcidComposition()
  })

  suppressWarnings({
    ncbi_ids <- datasets %>%
      purrr::map(~ as.integer(.x[["NCBI_ID"]])) %>% # as.integer helps to remove invalid NCBI IDs
      purrr::flatten_int() %>%
      unique() %>%
      .[!is.na(.)] %>%
      sort(decreasing = TRUE)

    taxonomies <- suppressMessages(taxize::classification(ncbi_ids, db = "ncbi", batch_size = 2000)) # the larger the batch size, the faster

  })

  ## The next step is necessary because invalid IDs might cause that
  ## individual outputs and their names don't match
  new_names <- purrr::map_chr(taxonomies, ~tail(.x[["id"]], 1))
  names(taxonomies) <- new_names

  ranks <- taxonomies %>%
    purrr::map_chr(~tail(.x$rank, 1))
  ranks_table <- tibble::tibble(NCBI_ID = names(ranks), rank = ranks)

  taxonomy_table <- cbind(taxonomies)

  taxonomy_table <- taxonomy_table %>%
    dplyr::select_at(grep(tax_regex, colnames(taxonomy_table), value = TRUE)) %>%
    dplyr::rename(NCBI_ID = query) %>%
    dplyr::left_join(ranks_table, by = "NCBI_ID") %>%
    dplyr::relocate("NCBI_ID", "rank") %>%
    dplyr::distinct()

  cache <- .getCache()

  rids <- BiocFileCache::bfcinfo(cache) %>%
    dplyr::filter(rname == "taxonomy_table") %>%
    dplyr::pull(rid)
  if (length(rids) > 0)
    BiocFileCache::bfcremove(cache, rids)

  save_path <- BiocFileCache::bfcnew(cache, "taxonomy_table", ext = ".tsv")
  readr::write_tsv(taxonomy_table, file = save_path, quote = "all")

  message("A new taxonomy table has been saved to cache.")

}

#' Taxonomy table
#'
#' \code{.getTaxonomyTable} imports a taxonomy table of all of the NCBI IDs
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
#' @keywords internal
#'
.getTaxonomyTable <- function(update = FALSE) {

  cache <- .getCache()

  rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_table", field = "rname", exact = TRUE) %>%
    dplyr::pull(rid)

  if (!length(rid)) {

    cat("There is no taxonomy table saved in cache. Creating one. This might take a few minutes.", sep = "\n")
    .createTaxonomyTable()
    rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_table", field = "rname", exact = TRUE) %>%
      dplyr::pull(rid)

  } else if (isTRUE(update)) {

    cat("Update parameter was set to TRUE. Creating a new taxonomy table. This might take a few minutes.", sep = "\n")
    .createTaxonomyTable()
    rid <- BiocFileCache::bfcquery(cache, query = "taxonomy_table", field = "rname", exact = TRUE) %>%
      dplyr::pull(rid)

  }

  rpath <- BiocFileCache::bfcrpath(cache, rids = rid)
  readr::read_tsv(rpath, show_col_types = FALSE)

}
