## code to prepare `sysdata.rda` dataset goes here

library(taxizedb)
library(bugphyzz)
library(purrr)
library(dplyr)

phys <- physiologies()

# ranks and parents -------------------------------------------------------
ncbi_ids <- phys |>
  map( ~ pull(.x, NCBI_ID)) |>
  flatten_chr() |>
  unique() |>
  {\(y) y[!is.na(y)]}() |>
  sort(decreasing = TRUE)

## This might change to taxize instead (might be slower)
taxonomies <- taxizedb::classification(ncbi_ids, db = "ncbi")

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

map(taxonomies, ~ filter(.x, rank %in% valid_ranks))







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













## Packages ####

# if (!requireNamespace('BiocManager', quietly = TRUE)) {
  # install.packages('BiocManager')
# }

## sdgamboa/taxPPro will be waldronlab/taxPPro in the future
# BiocManager::install('sdgamboa/taxPPro') # maybe use force = TRUE

## create taxonomyAnnotations  and ranks_parents ####
## In this taxonomyAnnotations, only Bacteria and Archaea organisms are
## present.

library(taxPPro)
library(purrr)
library(dplyr)

## Get all ncbi ids in bugphyzz ####
## First, get all the physiiologies in the spreadsheets
fname <- system.file('extdata/links.tsv', package = 'bugphyzz')
links <- read.table(fname, header = TRUE, sep = '\t')
output <- vector('list', nrow(links))
for (i in seq_along(output)) {
  phys_name <- links[i, 'physiology', drop = FALSE][[1]]
  message('Importing ', phys_name)
  names(output)[i] <- phys_name
  link <- links[i, 'link', drop = FALSE][[1]]
  output[[i]] <- read.csv(link, header = TRUE)
}

spreadsheets_ids <- lapply(output, function(x) {
  res <- as.character(x$NCBI_ID)
  if ('Parent_NCBI_ID' %in% colnames(x)) {
    res_ <- as.character(x$NCBI_ID)
    res <- unique(res, res_)
  }
  return(res)
}) |>
  unlist() |>
  unique()

bacdive <- bugphyzz:::.getBacDive() |>
  bugphyzz:::.reshapeBacDive()

bacdive_ids <- lapply(bacdive, function(x) {
  res <- as.character(x$NCBI_ID)
  if ('Parent_NCBI_ID' %in% colnames(x)) {
    res_ <- as.character(x$Parent_NCBI_ID)
    res <- unique(res, res_)
  }
  return(res)
}) |>
  unlist() |>
  unique()
ncbi_ids <- unique(spreadsheets_ids, bacdive_ids)

# Get NCBI taxonomy ------------------------------------------------------
ncbi <- get_ncbi_taxonomy(force_download = TRUE)
colnames(ncbi)[which(colnames(ncbi) == 'kingdom')] <- 'superkingdom'
ranks <- c('superkingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'strain')
taxnames <- ncbi[ncbi$Rank %in% ranks,]
x <- taxnames$Taxon_name
names(x) <- taxnames$NCBI_ID
y <- taxnames$Rank
names(y) <- taxnames$NCBI_ID

## ranks_parents
ranks_parents <- taxnames[,c('NCBI_ID', 'Rank', 'Parent_NCBI_ID')]
ranks_parents$Parent_name <- x[ranks_parents$Parent_NCBI_ID]
ranks_parents$Parent_rank <- y[ranks_parents$Parent_NCBI_ID]
ranks_parents <- ranks_parents[ranks_parents$NCBI_ID %in% ncbi_ids,]
ranks_parents$NCBI_ID <- as.character(ranks_parents$NCBI_ID)

## taxonomyAnnotations
colnames(taxnames) <- paste0('bugphyzz_', colnames(taxnames))
taxa <- taxnames$bugphyzz_NCBI_ID
names(taxa) <- taxnames$bugphyzz_Taxon_name
taxids <- modify_at(.x = taxnames, .at = ranks, .f = ~ taxa[.x])
pos <- which(colnames(taxids) %in% paste0('bugphyzz_', ranks))
colnames(taxids)[pos] <- paste0(colnames(taxids)[pos], '_id')
taxonomyAnnotations <- left_join(taxnames, taxids)
taxonomyAnnotations$bugphyzz_Taxon_name <- NULL
taxonomyAnnotations$bugphyzz_Parent_NCBI_ID <- NULL
taxonomyAnnotations <- taxonomyAnnotations[taxonomyAnnotations$bugphyzz_NCBI_ID %in% ncbi_ids,]
taxonomyAnnotations$bugphyzz_NCBI_ID <- as.character(taxonomyAnnotations$bugphyzz_NCBI_ID)

# names -------------------------------------------------------------------
bacdive_phys_names <- names(bacdive)

## Save data -------------------------------------------------------------
usethis::use_data(
  ranks_parents,
  taxonomyAnnotations,
  bacdive_phys_names,
  overwrite = TRUE, internal = TRUE
)
