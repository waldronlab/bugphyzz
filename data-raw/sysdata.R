## code to prepare `sysdata.rda` dataset goes here

## taxonomyAnnotations ---------------------------------------------------

valid_ranks = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus", "species", "strain")

tax_regex <- valid_ranks %>%
  c("query") %>%
  paste0(., collapse = "|") %>%
  paste0("^(", ., ")(_id)*$")

suppressMessages({
  datasets <- bugphyzz::physiologies("all")
  datasets[["fatty acid composition"]] <- bugphyzz::fattyAcidComposition()
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

taxonomyAnnotations <- taxonomy_table %>%
  dplyr::select_at(grep(tax_regex, colnames(taxonomy_table), value = TRUE)) %>%
  dplyr::rename(NCBI_ID = query) %>%
  dplyr::left_join(ranks_table, by = "NCBI_ID") %>%
  dplyr::relocate("NCBI_ID", "rank") %>%
  dplyr::distinct() %>%
  magrittr::set_colnames(paste0("bugphyzz_", colnames(.)))

## Save data -------------------------------------------------------------

usethis::use_data(taxonomyAnnotations, overwrite = TRUE, internal = TRUE)
