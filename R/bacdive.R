
## Main function for importing BacDive
.getBacDive <- function() {
  bacdive_data <- .importBacDiveExcel()
  colnames(bacdive_data) <- .changeBDColNames(colnames(bacdive_data))
  .getTidyBD(bacdive_data)
}

## Helper function for .getBacDive
.importBacDiveExcel <- function(verbose = TRUE) {
  if (verbose)
    message('Importing BacDive...')
  url <- 'https://docs.google.com/spreadsheets/d/1smQTi1IKt4wSGTrGTW25I6u47M5txZkq/export?format=csv'
  bacdive_data <- .cleanBD(utils::read.csv(url))
  bacdive_data
}

## Helper function for .getBacDive
.cleanBD <- function(df) {
  x_cols <- colnames(df)[grep('X', colnames(df))]
  output <- vector('list', length(x_cols))
  for (i in seq_along(x_cols)) {
    output[[i]] <- which(df[[x_cols[i]]] != '')
  }
  row_numbers <- sort(unique(unlist(output)))
  df[-row_numbers, !colnames(df) %in% x_cols]
}

## Helper function for .getBacDive
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

## Helper function for .getBacDive
.getTidyBD <- function(bacdive_data) {
  bacdive_data |>
    tidyr::pivot_longer(
      cols = .data$gram_stain:tidyr::last_col(),
      names_to = 'Attribute', values_to = 'Attribute_value'
    ) |>
    dplyr::filter(.data$Attribute_value != '') |>
    dplyr::mutate(Attribute = gsub('_', ' ', .data$Attribute)) |>
    dplyr::mutate(
      ## TODO Matching of attribute to those in the spreadsheets goes here
      Attribute = dplyr::case_when(
        .data$Attribute == 'oxygen tolerance' ~ 'aerophilicity',
        .data$Attribute == 'cell shape' ~ 'shape',
        .data$Attribute == 'pathogenicity animal' ~ 'animal pathongen',
        .data$Attribute == 'sample type' ~ 'isolation site',
        TRUE ~ .data$Attribute
      )
    ) |>
    dplyr::distinct()
}

## Function for getting a list of data.frames (one per attribute)
.reshapeBD <- function(df) {

  split_df <- split(df, factor(df[['Attribute']]))

  ## biosafey level and biosafety level comment should be joined
  ## biosafey level comment should then be removed
  biosafety_level <- split_df[['biosafety level']]
  biosafety_level_comment <- split_df[['biosafety level comment']]
  col_pos <- which(colnames(biosafety_level_comment) == 'Attribute_value')
  colnames(biosafety_level_comment)[col_pos] <- 'Note'
  biosafety_level <- dplyr::left_join(
    biosafety_level, biosafety_level_comment[,c('BacDive_ID', 'Note')],
    by = 'BacDive_ID'
  )
  split_df[['biosafety level']] <- biosafety_level
  split_df[['biosafety level comment']] <- NULL

  return(split_df)
}

