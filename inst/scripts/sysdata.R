
library(taxizedb)
library(bugphyzz)
library(purrr)
library(dplyr)
library(magrittr)
library(stringr)

getParentRank <- function(x) {
  ranks <- taxizedb::taxid2rank(x, db = 'ncbi', verbose = FALSE)
  lowest_ranks <- c(
    'biotype', 'isolate', 'serogroup', 'serotype', 'strain', 'subspecies'
  )
  dplyr::case_when(
    ranks %in% lowest_ranks ~ 'species',
    ranks == 'species' ~ 'genus',
    ranks == 'genus' ~ 'family',
    TRUE ~ NA
  )
}

tax_ranks <- c(
  "superkingdom", "phylum", "class", "order", "family", "genus",
  "species", "strain"
)

phys <- physiologies()

ncbi_ids <- phys |>
  map( ~ pull(.x, NCBI_ID)) |>
  flatten_chr() |>
  unique() |>
  str_squish() |>
  str_to_lower() |>
  {\(y) y[y != 'unknown']}() |>
  {\(y) y[!is.na(y)]}() |>
  sort(decreasing = TRUE)

tim <- system.time({
  taxonomies <- taxizedb::classification(ncbi_ids, db = "ncbi")
  lgl_vct <- !map_lgl(taxonomies, ~ all(is.na(.x)))
  taxonomies <- taxonomies[lgl_vct]
  ncbi_ids <- ncbi_ids[lgl_vct]
})
print(tim)

## Check names and taxid match
all(names(taxonomies) == map_chr(taxonomies, ~ as.character(tail(.x$id, 1))))

parents_ranks <- getParentRank(ncbi_ids)
lgl_vct <- !is.na(parents_ranks)
ncbi_ids <- ncbi_ids[lgl_vct]
parents_ranks <- parents_ranks[lgl_vct]
taxonomies <- taxonomies[lgl_vct]

parent_ids <- map2(taxonomies, parents_ranks,  ~{
  parent_rank <- .x |>
    filter(rank %in% tax_ranks) |>
    pull(rank) |>
    {\(y) y[-length(y)]}() |> ## Need to remove the current rank
    tail(1)
  parent_id <- .x |>
    filter(rank %in% tax_ranks) |>
    pull(id) |>
    {\(y) y[-length(y)]}() |> ## Need to remove the current rank
    tail(1)
  names(parent_id) <- parent_rank
  ifelse(names(parent_id) == .y, parent_id, NA)
})

lgl_vct <- !is.na(parent_ids)
ncbi_ids <- ncbi_ids[lgl_vct]
parent_ids <- parent_ids[lgl_vct]

ranks_parents <- data.frame(
  NCBI_ID = ncbi_ids,
  # Taxon_name = taxizedb::taxid2name(ncbi_ids, db = 'ncbi'),
  Rank = taxizedb::taxid2rank(ncbi_ids, db = 'ncbi'),
  Parent_NCBI_ID = unlist(parent_ids),
  Parent_name = taxizedb::taxid2name(unlist(parent_ids), db = 'ncbi'),
  Parent_rank = taxizedb::taxid2rank(unlist(parent_ids), db = 'ncbi')
)
rownames(ranks_parents) <- NULL

# BacDive -----------------------------------------------------------------
bacdive <- bugphyzz:::.getBacDive() |>
  bugphyzz:::.reshapeBacDive()
bacdive_phys_names <- names(bacdive)

## Save data -------------------------------------------------------------
usethis::use_data(
  ranks_parents,
  bacdive_phys_names,
  overwrite = TRUE, internal = TRUE
)
