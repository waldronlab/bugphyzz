
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
## This function works over several datasets.
.reshapeBD <- function(df) {

  df[['Attribute_source']] <- 'BacDive'
  split_df <- split(df, factor(df[['Attribute']]))

  ## Attributes that must be changed from character to logical (simplest fix)
  attr_names <- c(
    'aerophilicity', 'biosafety level', 'colony color', 'country',
    'cultivation medium used', 'geographic location', 'isolation site',
    'shape'
  )

  for (i in seq_along(attr_names)) {
    split_df[[attr_names[i]]] <- .catToLog(split_df[[attr_names[i]]])
  }

  ## animal pathogen
  pos <- names(split_df) == 'animal pathongen'
  names(split_df)[pos] <- 'animal pathogen'
  x_ <- split_df[['animal pathogen']][['Attribute_value']]
  x_ <- ifelse(x_ == "yes, in single cases", "yes", x_)
  x_ <- dplyr::case_when(x_ == 'yes' ~ TRUE, x_ == 'no' ~ FALSE)
  split_df[['animal pathogen']][['Attribute_value']] <- x_
  split_df[['animal pathogen']][['Attribute_group']] <- 'animal pathogen'
  split_df[['animal pathogen']][['Attribute_type']] <- 'logical'

  ## cultivation medium used - growth medium
  pos <- names(split_df) == 'cultivation medium used'
  names(split_df)[pos] <- 'growth medium'
  split_df[['growth medium']][['Attribute_group']] <- 'growth medium'

  ## growth temperature
  ## culture temperature
  ## culture temperature growth
  ## culture temperature range (ignore)
  ## culture temperature type (ignore)
  split_df[['culture temperature range']] <- NULL
  split_df[['culture temperature type']] <- NULL
  a <- split_df[['culture temperature']]
  b <- split_df[['culture temperature growth']]
  b_ <- b[,c('BacDive_ID', 'Attribute_value')]
  colnames(b_)[2] <- 'growth'
  ab <- dplyr::left_join(a, b_, by = 'BacDive_ID')
  ab <- ab[ab[['growth']] == 'positive',]
  ab[['growth']] <- NULL
  ab[['Attribute_group']] <- 'growth temperature'
  ab[['Attribute_type']] <- 'numeric'
  ab[['Attribute']] <- 'growth temperature'
  split_df[['growth temperature']] <- ab
  split_df[['culture temperature']] <- NULL
  split_df[['culture temperature growth']] <- NULL

  ## gram stain
  gs <- split_df[['gram stain']]
  gs[['Attribute']] <- paste(gs[['Attribute']], gs[['Attribute_value']])
  gs[['Attribute_value']] <- TRUE
  split_df[['gram stain']] <- gs

  ## halophily
  valid_terms <- c(
    'NaCl', 'KCl', 'MgCl2', 'MgCl2x6H2O', 'Na\\+', 'MgSO4x7H2O', 'Na2SO4',
    'Sea salts', 'Chromium \\(Cr6\\+\\)'
  )
  regex <- paste0('(', paste0(valid_terms, collapse = '|'), ')')
  split_df[['halophily']] <- split_df[['halophily']] |>
    dplyr::mutate(Attribute_value = strsplit(.data$Attribute_value, ';')) |>
    tidyr::unnest(cols = 'Attribute_value') |>
    dplyr::filter(!grepl('no growth', .data$Attribute_value)) |>
    dplyr::mutate(
      Attribute_value = stringr::str_squish(.data$Attribute_value),
      Attribute_value = sub('NaCL', 'NaCl', .data$Attribute_value),
      Attribute_value = sub('Marine', 'Sea', .data$Attribute_value),
      Attribute_value = sub('Salts', 'salts', .data$Attribute_value)
    ) |>
    dplyr::filter(grepl(regex, .data$Attribute_value)) |>
    dplyr::mutate(
      Attribute = stringr::str_extract(.data$Attribute_value, regex),
      Unit = .data$Attribute_value |>
        stringr::str_extract(' [<>]??[0-9]+\\.??[0-9]*.*') |>
        stringr::str_squish() |>
        stringr::str_remove('^.* '),
      Attribute_value = .data$Attribute_value |>
        stringr::str_extract(' [<>]??[0-9]+\\.??[0-9]*.*') |>
        stringr::str_squish() |>
        stringr::str_remove(' .*$'),
      Attribute_group = 'halophily',
      Attribute_type = 'range'
    ) |>
    dplyr::filter(!grepl('[0-9]', .data$Unit)) |>
    dplyr::distinct()

  ## hemolysis
  split_df[['hemolysis']] <- split_df[['hemolysis']] |>
    dplyr::mutate(
      Attribute_value = strsplit(.data$Attribute_value, ';|/')
    ) |>
    tidyr::unnest(.data$Attribute_value) |>
    dplyr::mutate(Attribute_value = stringr::str_squish(.data$Attribute_value)) |>
    dplyr::filter(.data$Attribute_value != '') |>
    dplyr::mutate(
      Attribute = .data$Attribute_value,
      Attribute_value = TRUE,
      Attribute_group = 'hemolysis',
      Attribute_type = 'logical'
    )

  ## incubation period
  ip <- split_df[['incubation period']]
  ip[['Unit']] <- 'days'
  ip[['Attribute_value']] <- ip[['Attribute_value']] |>
    stringr::str_remove(' .*day.*$') |>
    stringr::str_squish()
  ip[['Attribute_group']] <- 'incubation period'
  ip[['Attribute_type']] <- 'range'
  split_df[['incubation period']] <- ip

  ## motility
  split_df[['motility']] <- split_df[['motility']] |>
    dplyr::mutate(
      Attribute_value = dplyr::case_when(
        .data[['Attribute_value']] == 'yes' ~ TRUE,
        .data[['Attribute_value']] == 'no' ~ FALSE
      )
    )

  ## pathogenicity human
  pat <- split_df[['pathogenicity human']]
  pat[['Note']] <- stringr::str_extract(pat[['Attribute_value']], 'in single cases')
  pat[['Note']] <- ifelse(is.na(pat[['Note']]), "", pat[['Note']])
  pat[['Attribute_value']] <- ifelse(grepl('^yes', pat[['Attribute_value']]), TRUE, NA)
  pat <- pat[!is.na(pat[['Attribute_value']]),]
  split_df[['pathogenicity human']] <- pat

  return(split_df)
}


.catToLog <- function(df) {
  df[['Attribute_group']] <- df[['Attribute']]
  df[['Attribute']] <- df[['Attribute_value']]
  df[['Attribute_value']] <- TRUE
  df[['Attribute_type']] <- 'logical'
  return(df)
}

