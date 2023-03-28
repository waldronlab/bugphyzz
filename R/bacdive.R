
## Main function for importing BacDive
.getBacDive <- function(verbose = FALSE ) {
  bacdive_data <- .importBacDiveExcel(verbose = verbose)
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
.reshapeBacDive <- function(df) {

  df[['Attribute_source']] <- 'BacDive'
  split_df <- split(df, factor(df[['Attribute']]))

  ## Attributes that must be changed from character to logical (simplest fix)
  attr_names <- c(
    'aerophilicity', 'country',
    'cultivation medium used', 'geographic location', 'isolation site',
    'shape'
    ## colony color (delete)
  )

  for (i in seq_along(attr_names)) {
    split_df[[attr_names[i]]] <- .catToLog(split_df[[attr_names[i]]])
  }

  ## aerophilicity
  aer <- split_df[['aerophilicity']]
  aer$Attribute <- case_when(
    aer$Attribute == 'aerobe' ~ 'aerobic',
    aer$Attribute == 'anaerobe' ~ 'anaerobic',
    aer$Attribute == 'facultative anaerobe' ~ 'facultatively anaerobic',
    aer$Attribute == 'microaerophile' ~ 'microaerophilic',
    aer$Attribute == 'obligate anaerobe' ~ 'obligately anaerobic',
    TRUE ~ aer$Attribute
  )
  split_df[['aerophilicity']]

  ## animal pathogen
  pos <- names(split_df) == 'animal pathongen'
  names(split_df)[pos] <- 'animal pathogen'
  x_ <- split_df[['animal pathogen']][['Attribute_value']]
  x_ <- ifelse(x_ == "yes, in single cases", "yes", x_)
  x_ <- dplyr::case_when(x_ == 'yes' ~ TRUE, x_ == 'no' ~ FALSE)
  split_df[['animal pathogen']][['Attribute_value']] <- x_
  split_df[['animal pathogen']][['Attribute_group']] <- 'animal pathogen'
  split_df[['animal pathogen']][['Attribute']] <- 'animal pathogen'
  split_df[['animal pathogen']][['Attribute_type']] <- 'logical'

  ## biosafety level
  y <- split_df[['biosafety level comment']][, c('BacDive_ID', 'Attribute_value')]
  colnames(y)[2] <- 'Note'
  x <- dplyr::left_join(split_df[['biosafety level']], y, by = 'BacDive_ID')
  x[['Attribute_value']] <- paste0('biosafety level ', x[['Attribute_value']])
  x[['Attribute']] <- x[['Attribute_value']]
  x[['Attribute_value']] <- TRUE
  x[['Attribute_group']] <- 'biosafety level'
  x[['Attribute_type']] <- 'logical'
  split_df[['biosafety level']] <- x
  split_df[['biosafety level comment']] <- NULL

  ## colony color
  split_df[['colony color']] <- NULL

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
  ab[['Attribute_type']] <- 'range'
  ab[['Attribute']] <- 'growth temperature'
  split_df[['growth temperature']] <- ab
  split_df[['culture temperature']] <- NULL
  split_df[['culture temperature growth']] <- NULL

  ## gram stain
  gs <- split_df[['gram stain']]
  gs[['Attribute']] <- paste(gs[['Attribute']], gs[['Attribute_value']])
  gs[['Attribute_value']] <- TRUE
  gs[['Attribute_group']] <- 'gram stain'
  gs[['Attribute_type']] <- 'logical'
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
  split_df[['incubation period']] <- NULL
  # ip <- split_df[['incubation period']]
  # ip[['Unit']] <- 'days'
  # ip[['Attribute_value']] <- ip[['Attribute_value']] |>
  #   stringr::str_remove(' .*day.*$') |>
  #   stringr::str_squish()
  # ip[['Attribute_group']] <- 'incubation period'
  # ip[['Attribute_type']] <- 'range'
  # split_df[['incubation period']] <- ip

  ## motility
  split_df[['motility']] <- split_df[['motility']] |>
    dplyr::mutate(
      Attribute_value = dplyr::case_when(
        .data[['Attribute_value']] == 'yes' ~ TRUE,
        .data[['Attribute_value']] == 'no' ~ FALSE
      )
    )
  split_df[['motility']][['Attribute_group']] <- 'motility'
  split_df[['motility']][['Attribute_type']] <- 'logical'

  ## pathogenicity human
  pat <- split_df[['pathogenicity human']]
  pat[['Note']] <- stringr::str_extract(pat[['Attribute_value']], 'in single cases')
  pat[['Note']] <- ifelse(is.na(pat[['Note']]), "", pat[['Note']])
  pat[['Attribute_value']] <- ifelse(grepl('^yes', pat[['Attribute_value']]), TRUE, NA)
  pat <- pat[!is.na(pat[['Attribute_value']]),]
  pat[['Attribute_group']] <- 'pathogenicity human'
  pat[['Attribute_type']] <- 'logical'
  split_df[['pathogenicity human']] <- pat


  ## metabolite production
  mp <- split_df[['metabolite production']]
  mp <- mp |>
    dplyr::mutate(Attribute_value = strsplit(.data$Attribute_value, ';')) |>
    tidyr::unnest(.data$Attribute_value)
  x <- stringr::str_extract(mp[['Attribute_value']], '(yes|no)$')
  mp <- mp[which(!is.na(x)),]
  y <- stringr::str_extract(mp[['Attribute_value']], '(yes|no)$')
  mp[['Attribute']] <- mp[['Attribute_value']]
  mp[['Attribute_value']] <- ifelse(y == 'yes', TRUE , FALSE)
  mp[['Attribute']] <- sub(' (yes|no)$', '', mp[['Attribute']])
  mp[['Attribute_group']] <- 'metabolite utilization'
  mp[['Attribute_type']] <- 'logical'
  split_df[['metabolite production']] <- mp

  ## metabolite utilization
  pos <- names(split_df) == 'metabolite utiilization'
  names(split_df)[pos] <- 'metabolite utilization'
  mu <- split_df[['metabolite utilization']]
  mu <- mu |>
    dplyr::mutate(Attribute_value = strsplit(.data$Attribute_value, ';')) |>
    tidyr::unnest(.data$Attribute_value) |>
    dplyr::mutate(Attribute_value = stringr::str_squish(.data$Attribute_value))
  x <- sub('^.* (\\+|-|\\+/-) *.*$', '\\1', mu[['Attribute_value']])
  y <- ifelse(!x %in% c('+', '-', '+/-'), NA, x)
  mu <- mu[which(!is.na(y)),]
  mu[['Attribute']] <- stringr::str_remove(mu[['Attribute_value']], ' (\\+|-|\\+/-) *.*$')
  mu[['Note']] <- sub('^.*(\\+|-|\\+/-) ', '', mu[['Attribute_value']])
  mu[['Note']] <- paste('kind of utilization tested:', mu[['Note']])
  y <- y[!is.na(y)]
  mu[['Attribute_value']] <- dplyr::case_when(
    y == '+' ~ 'TRUE',
    y == '-' ~ 'FALSE',
    y == '+/-' ~ 'TRUE/FALSE'
  )
  mu <- mu |>
    dplyr::mutate(Attribute_value = strsplit(.data$Attribute_value, '/')) |>
    tidyr::unnest(.data$Attribute_value) |>
    dplyr::mutate(Attribute_value = as.logical(.data$Attribute_value))
  mu[['Attribute_group']] <- 'metabolite utilization'
  mu[['Attribute_type']] <- 'logical'
  split_df[['metabolite utilization']] <- mu

  ## spore formation
  sf <- split_df[['spore formation']]
  sf <- sf |>
    dplyr::mutate(
      Attribute_value = dplyr::case_when(
        .data[['Attribute_value']] == 'yes' ~ TRUE,
        .data[['Attribute_value']] == 'no' ~ FALSE
      ),
      Attribute_group = 'spore formation',
      Attribute_type = 'logical'
    ) |>
    dplyr::filter(!is.na(.data$Attribute_value))
  split_df[['spore formation']] <- sf

  split_df <- lapply(split_df, function(x) {
    x <- as.data.frame(x)
    x[['NCBI_ID']] <- as.character(x[['NCBI_ID']])
    x[['Parent_NCBI_ID']] <- as.character(x[['Parent_NCBI_ID']])
    x[['Frequency']] <- 'always'
    x <- x[!is.na(x[['Attribute_value']]),]
    if (unique(x[['Attribute_type']]) == 'numeric') {
      x <- .numericToRange(x)
    } else if (unique(x[['Attribute_type']] == 'range')) {
      x <- .modifyRange(x)
    }
    x <- x[,purrr::map_lgl(x, ~ !all(is.na(.x)))]
    x <- dplyr::distinct(x)
    as.data.frame(x)
    # x <- .addSourceInfo(x)
  })

  return(split_df)
}

.catToLog <- function(df) {
  df[['Attribute_group']] <- df[['Attribute']]
  df[['Attribute']] <- df[['Attribute_value']]
  df[['Attribute_value']] <- TRUE
  df[['Attribute_type']] <- 'logical'
  return(df)
}

