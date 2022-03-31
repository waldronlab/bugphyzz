## code to prepare `ProTraits` dataset goes here

if (packageVersion("taxize") < "0.9.99.947") { # a bug was fixed in this version
  devtools::install_github("ropensci/taxize", force = TRUE)
}

suppressMessages({
  library(tidyr)
  library(dplyr)
  library(BiocFileCache)
  library(taxize)
  library(googlesheets4)
})

## A helper function ####
## This function is for formatting the output of taxize/taxizedb
taxize_classification_to_taxonomy_table <- function(tax, id_type = "name") {
  valid_ranks <- c(
    "superkingdom", "class", "order", "family", "genus", "species"
  )
  query_names <- as.data.frame(names(tax))
  colnames(query_names) <- "query"
  taxonomy_list <- lapply(tax, function(x) {
    if (any(is.na(x))) {
      data.frame(
        kingdom = NA, class = NA, order = NA, family = NA,
        genus = NA, species = NA
      )
    } else {
      df <- x[x$rank %in% valid_ranks, ]
      df <- df[,c("rank", id_type)]
      df <- as.data.frame(t(df))
      col_names <- as.character(df[1,])
      df <- as.data.frame(df[-1,])
      colnames(df) <- col_names
      colnames(df)[colnames(df) == "superkingdom"] <- "kingdom"
      rownames(df) <- NULL
      df
    }
  })
  taxonomy_table <- taxonomy_list %>%
    dplyr::bind_rows()
  cbind(query_names, taxonomy_table)
}

## Download and import data ####

rname <- "bugphyzz_protraits"
url <- "http://protraits.irb.hr/data/ProTraits_binaryIntegratedPr0.90.txt"

bfc_path <- bfccache()
bfc <- BiocFileCache(bfc_path)

resources <- bfcquery(bfc, query = rname, field = "rname", exact = TRUE)

if (nrow(resources) == 0) {
  message("File not in cache. Downloading...")
  rpath <- bfcadd(bfc, rname = rname, fpath = url, download = TRUE)
} else if(nrow(resources) == 1) {
  message("File alredy in cache.")
  rid <- pull(resources, var = "rid")
  rpath <- bfcpath(bfc, rid)
}

data <- vroom::vroom(rpath, delim = "\t", show_col_types = FALSE)

## Get taxonomic classification

tax_class <- classification(data$Tax_ID, db = 'ncbi') # very recommended to have an API key

parent_data <- lapply(tax_class, function(x) {
  # get rank
  rank <- tail(x$rank, 1)

  # get parent_name
  parents <- x[-nrow(x), ]
  rank_names <- c(
    "superkingdom", "phylum", "class", "order", "family", "genus", "species"
  )
  parents <- parents[parents$rank %in% rank_names,]
  output <- parents[nrow(parents), ]
  names(output) <- c("Parent_name", "Parent_rank", "Parent_NCBI_ID")
  output$Rank <- rank
  output
}) %>%
  bind_rows(.id = "NCBI_ID")

## Columns order
## Required columns
extra_columns <- c(
  "Rank", "Parent_name", "Parent_NCBI_ID", "Parent_rank"
)
required_columns <- bugphyzz:::.requiredColumns()

if (!any(extra_columns %in% required_columns)) {
  required_columns <- c(required_columns, extra_columns)
}

## transform data
proTraits <- data %>%
  pivot_longer(
    names_to = "Attribute", values_to = "Attribute_value",
    cols = 3:last_col()
  ) %>%
  mutate(
    Attribute_value = ifelse(Attribute_value == "?", NA, Attribute_value) %>%
      as.integer() %>%
      as.logical()
  ) %>%
  filter(!is.na(Attribute_value)) %>%
  rename(Taxon_name = Organism_name, NCBI_ID = Tax_ID) %>%
  mutate(
    Attribute_source = "ProTratis",
    Confidence_interval = "always", # Because all are at 90%
    Genome_ID = NA,
    Accession_ID = NA,
    NCBI_ID = as.character(NCBI_ID),
    Evidence = 'COMP'
  ) %>%
  left_join(parent_data, by = "NCBI_ID") %>%
  relocate(all_of(required_columns))


## Locate folder with attributes
## This path might change depending on user permissions???

# proTraits_ss <- 'Waldron Lab/Kelly Eckenrode/BugPhyzz/Attributes/ProTraits'
# proTraits_ss_metadata <- drive_get(path = proTraits_ss)
# if (nrow(proTraits_ss_metadata) == 1) {
#   proTraits_ss_id <- pull(proTraits_ss_metadata, "id")
# }

## I created an empty sheet manually in google drive
proTraits_ss_id <- '1KvySjXtRNTTT8kEA28OlMv00rMpe_nsxJ4C3cxJgWdM'

## Writing using write_sheet can take a really long time (~10 min)
## Maybe is better to write the sheet and upload to the google drive
# write_sheet(data = proTraits, ss = proTraits_ss_id, sheet = "proTraits")

