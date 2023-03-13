
## code to prepare `sysdata.rda` dataset goes here

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
