.physiologiesBD <- function() {
  ## TODO
}

.importBacDive <- function() {
  url <- 'https://docs.google.com/spreadsheets/d/1smQTi1IKt4wSGTrGTW25I6u47M5txZkq/export?format=csv'
  data <- .cleanBD(utils::read.csv(url))
  colnames(data) <- .changeBDColNames(colnames(data))
  data |>
    .tidyBD()
}

.cleanBD <- function(df) {
  x_cols <- colnames(df)[grep('X', colnames(df))]
  output <- vector('list', length(x_cols))
  for (i in seq_along(x_cols)) {
    output[[i]] <- which(df[[x_cols[i]]] != '')
  }
  row_numbers <- sort(unique(unlist(output)))
  df[-row_numbers, !colnames(df) %in% x_cols]
}

.changeBDColNames <- function(x) {
  dplyr::case_when(
    x == 'bacdive_id' ~ 'BacDive_ID',
    x == 'taxon_name' ~ 'Taxon_name',
    x == 'ncbi_id' ~ 'NCBI_ID',
    x == 'rank' ~ 'Rank',
    x == 'parent_taxon_name' ~ 'Parent_name',
    x == 'parent_ncbi_id' ~ 'Parent_NCBI_ID',
    x == 'parent_rank' ~ 'Parent_rank',
    x == 'sequence_16S_ncbi_id' ~ 'Seq16S_NCBI_ID',
    x == 'sequence_genome_ncbi_id' ~ 'Genome_ID',
    x == 'type_strain' ~ 'Type_strain',
    TRUE ~ x
  )
}

.tidyBD <- function(bd) {
  bd |>
    tidyr::pivot_longer(
      cols = .data$gram_stain:tidyr::last_col(),
      names_to = 'Attribute', values_to = 'Attribute_value'
    ) |>
    dplyr::filter(Attribute_value != '')
}





