## code to prepare `sysdata.rda` dataset goes here

library(taxizedb)
library(bugphyzz)
library(purrr)
library(dplyr)
library(magrittr)

phys <- physiologies()
phys[["fatty acid composition"]] <- fattyAcidComposition()
phys <- discard(phys, ~ !"Evidence" %in% colnames(.x))


# ranks and parents -------------------------------------------------------
ncbi_ids <- phys %>%
    ## as.integer helps to remove invalid NCBI IDs
    purrr::map(~ as.integer(.x[["NCBI_ID"]])) %>%
    purrr::flatten_int() %>%
    unique() %>%
    .[!is.na(.)] %>%
    sort(decreasing = TRUE)

taxonomies <- taxizedb::classification(ncbi_ids, db = "ncbi")

## Check that names correspond to the right output
name_value <- names(taxonomies) == map_chr(
    taxonomies, ~ tryCatch(tail(.x$id, 1), error = function(e) NA)
)
invalid_taxonomy_ids <- name_value[is.na(name_value)]
sum(name_value, na.rm = TRUE) / length(name_value) * 100
## In the lines above, most names correspond to the right output

taxonomies <- taxonomies[!is.na(taxonomies)]

valid_ranks <- c(
    "superkingdom", "kingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
)

## Get parents
parents <- taxonomies %>%
  map( ~ {
  .x %>%
    head(-1) %>%
    filter(rank %in% valid_ranks) %>%
    tail(1) %>%
    set_colnames(paste0("Parent_", names(.)))
}) %>%
  bind_rows(.id = "NCBI_ID")

## Get ranks
ranks <- taxonomies %>%
  map_chr(~ tail(.x$rank, 1)) %>%
  as_tibble(rownames = "NCBI_ID") %>%
  set_colnames(c("NCBI_ID", "Rank"))

ranks_parents <- full_join(ranks, parents, by = "NCBI_ID") %>%
  mutate(NCBI_ID = as.integer(NCBI_ID)) %>%
  relocate("Parent_rank", .after = "Parent_id") %>%
  rename(Parent_NCBI_ID = Parent_id)


# full taxonomy annotations -----------------------------------------------
tax_regex <- valid_ranks %>%
  c("query") %>%
  paste0(., collapse = "|") %>%
  paste0("^(", ., ")(_id)*$")


attr(taxonomies, "class") <- "classification"
taxonomy_table <- cbind(taxonomies)

taxonomyAnnotations <- taxonomy_table %>%
  dplyr::select_at(grep(tax_regex, colnames(taxonomy_table), value = TRUE)) %>%
  dplyr::rename(NCBI_ID = query) %>%
  dplyr::left_join(ranks, by = "NCBI_ID") %>%
  dplyr::relocate("NCBI_ID", "Rank") %>%
  dplyr::rename(rank = Rank) %>%
  dplyr::distinct() %>%
  magrittr::set_colnames(paste0("bugphyzz_", colnames(.)))

ranks_parents <- purrr::modify_if(
  ranks_parents,
  .p = grepl("NCBI_ID", colnames(ranks_parents)),
  .f =  ~ as.integer(.x)
)

## Save data -------------------------------------------------------------
usethis::use_data(
  ranks_parents,
  taxonomyAnnotations,
  overwrite = TRUE, internal = TRUE
)

