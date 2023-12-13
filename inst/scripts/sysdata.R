
library(taxizedb)
library(bugphyzz)
library(purrr)
library(dplyr)
library(magrittr)
library(stringr)

phys <- physiologies()

ncbi_ids <- phys |>
  map( ~ pull(.x, NCBI_ID)) |>
  flatten_chr() |>
  unique() |>
  str_squish() |>
  str_to_lower() |>
  {\(y) y[!is.na(y)]}() |>
  {\(y) y[y != 'unknown']}() |>
  sort(decreasing = TRUE)

parent_ids <- getParentID(ncbi_ids)


taxonomies <- taxizedb::classification(ncbi_ids, db = "ncbi")
taxonomies <- discard(taxonomies, ~ all(is.na(.x))) # disc+arded taxids migth need to be updated

## Check names and taxid match
all(names(taxonomies) == map_chr(taxonomies, ~ as.character(tail(.x$id, 1))))

table(taxizedb::taxid2rank(names(taxonomies), db = 'ncbi'))


getParentID <- function(taxid) {
  below_sp <- c(
    'biotype', 'isolate', 'serotype', 'strain', 'subspecies'
  )
  rank <- taxizedb::taxid2rank(taxid, db = 'ncbi')
  parent_rank <- dplyr::case_when(
    rank %in% below_sp ~ 'species',
    rank == 'species' ~ 'genus',
    rank == 'genus' ~ 'family',
    TRUE ~ NA
  )
  remove_pos <- which(is.na(rank) | is.na(parent_rank))
  taxid <- taxid[-remove_pos]
  rank <- rank[-remove_pos]
  parent_rank <- parent_rank[-remove_pos]
  parent_id <- map2(.x = taxid, .y = parent_rank, ~ {
    taxizedb::taxa_at(x = .x, rank = .y, db = 'ncbi', verbose = FALSE)
  })
  data.frame(
    taxid = taxid,
    rank = rank,
    parent_it = parent_id,
    parent_rank = parent_rank
  )
}



taxizedb::taxa_at(x = '562', rank = 'family', db = 'ncbi', verbose = FALSE)

x = getParentID(names(taxonomies))

table(x, useNA = 'always')


x = map(taxonomies, ~ {
  count(.x, rank)
}) |>
  bind_rows()



## >>>>>>>> this is not part of the output <<<<<<<<<<<<<<<
## Check that names correspond to the right output
name_value <- names(taxonomies) == map_chr(
  taxonomies, ~ tryCatch(tail(.x$id, 1), error = function(e) NA)
)
invalid_taxonomy_ids <- name_value[is.na(name_value)]
sum(name_value, na.rm = TRUE) / length(name_value) * 100
## In the lines above, most names correspond to the right output
## >>>>>>>> this is not part of the output <<<<<<<<<<<<<<<

taxonomies <- taxonomies[!is.na(taxonomies)]
valid_ranks <- c(
  "superkingdom", "phylum", "class", "order", "family", "genus",
  "species", "strain"
)

taxonomies <- map(taxonomies, ~ filter(.x, rank %in% valid_ranks))
taxonomies <- discard(taxonomies, ~ nrow(.x) <= 2)

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
  .f =  ~ as.character(.x)
)

# BacDive -----------------------------------------------------------------
bacdive <- bugphyzz:::.getBacDive() |>
  bugphyzz:::.reshapeBacDive()
bacdive_phys_names <- names(bacdive)

## Save data -------------------------------------------------------------
usethis::use_data(
  ranks_parents,
  taxonomyAnnotations,
  bacdive_phys_names,
  overwrite = TRUE, internal = TRUE
)
