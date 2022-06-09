library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(bugphyzz)

bacdive_url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQT5lIjO0Zel_OBYNmQXGziCWOXZzqrJqwY6BWgNUkeeNNH4F7noM-oRWiZVTuu4mDX-YM_0eATXOaJ/pub?gid=1510921577&single=true&output=csv'
bacdive_data <- readr::read_csv(bacdive_url)

## Remove extra columsn due to parsing problems (if any)
extra_cols <- as.integer(na.omit(as.integer(sub("^...", "", colnames(bacdive_data)))))

if (length(extra_cols) > 0) {

  extra_data <- bacdive_data[,extra_cols]

  output <- vector("list", ncol(extra_data))
  for (i in seq_along(output)) {
    output[[i]] <- which(!is.na(extra_data[[i]]))
  }

  rows_to_correct <- unlist(output, recursive = TRUE) + 1 # adding one because of header

  keep_rows <- setdiff(1:nrow(bacdive_data), rows_to_correct)
  keep_cols <- setdiff(1:ncol(bacdive_data), extra_cols)

  bacdive_data <- bacdive_data[keep_rows, keep_cols]
}

## Convert all numeric columns to character (if any).
## These will be converted back to numeric later.
numeric_cols <- c(
  'Metabolite_Utilization_CHEBI_ID'
)
for (i in seq_along(bacdive_data)) {
  col_name <- colnames(bacdive_data)[i]
  if (col_name %in% numeric_cols) {
    bacdive_data[[i]] <- as.character(bacdive_data[[i]])
  }
}

## Convert data to long format and divide
bacdive_data_long <- bacdive_data %>%
  pivot_longer(
    cols = Gram_Stain:last_col(),
    names_to = 'Dataset', values_to = 'Attribute'
    ) %>%
  mutate(
    Attribute_source = 'BacDive'
  ) %>%
  rename(
    Taxon_name = Taxon_Name, Parent_name = Parent_Name,
    Parent_rank = Parent_Rank, Type_strain = Type_Strain,
    Full_scientific_name = Full_Scientific_Name
  ) %>%
  relocate(
    NCBI_ID, Taxon_name, Genome_ID, Accession_ID, Dataset, Attribute,
    Attribute_source
  ) %>%
  mutate(
    Dataset = str_replace_all(Dataset, '_', ' ') %>%
      str_to_sentence(),
    ## This is to make coincide names in the Dataset column and names in
    ## bugphyzz (when applicable).
    Dataset = case_when(
      Dataset == 'Cell shape' ~ 'shape',
      Dataset == 'Cultivation medium used' ~ 'growth medium',
      Dataset == 'Oxygen tolerance' ~ 'aerophilicity',
      TRUE ~ Dataset

    )
  )

numeric_cols_2 <- str_replace_all(numeric_cols, '_', ' ') %>%
  str_to_sentence()

bacdive_data_list <- bacdive_data_long %>%
  split(factor(.$Dataset)) %>%
    map2(.x = ., .y = names(.), ~ {

      attribute_name <- unique(.x$Dataset)
      if (attribute_name %in% numeric_cols_2) {
        .x$Attribute <- as.numeric(.x$Attribute)
      }

      .x <- filter(.x, !is.na(Attribute)) %>%
        distinct()

      if (is.character(.x$Attribute)) {
        .x$Attribute_value <- TRUE
      } else if (is.numeric(.x$Attribute)) {
        .x$Attribute_value <- .x$Attribute
        .x$Attribute <- .y
      }

      select(.x, -Dataset) %>%
        relocate(Attribute_value, .after = 'Attribute')
    })

bugphyzz <- physiologies()

output <- vector('list', length(bacdive_data_list))
names(output) <- names(bacdive_data_list)

for (i in seq_along(bacdive_data_list)) {
  for (j in seq_along(bugphyzz)) {
    name_bacdive <- names(bacdive_data_list)[i]
    name_bugphyzz <- names(bugphyzz)[j]
    if (name_bacdive == name_bugphyzz) {
      message(paste('joining', name_bacdive, 'in bacdive and', name_bugphyzz, 'in bugphyzz.'))
      bacdive_data_list[[i]]$NCBI_ID <- as.character(bacdive_data_list[[i]]$NCBI_ID)
      bugphyzz[[j]]$NCBI_ID <- as.character(bugphyzz[[j]]$NCBI_ID)
      output[[i]] <- bind_rows(bacdive_data_list[[i]], bugphyzz[[j]])
    } else {
      output[[i]] <- bacdive_data_list[[i]]
    }
  }
}

## Save all files as csv files

dirname <- '~/sandbox/'

if (!dir.exists(dirname)) {
  stop(paste('There is no directory named as', dirname), call. = FALSE)
} else {

  for (i in seq_along(output)) {

    fname <- paste0(
      dirname,
      str_replace_all(names(output)[i], ' ', '_'),
      '.csv'
    )

    write.table(
      x = output[[i]], file = fname,
      sep = 'csv', col.names = TRUE
    )
  }

}

