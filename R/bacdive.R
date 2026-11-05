
## Main function for importing BacDive
.getBacDive <- function(verbose = FALSE, forceDownload = FALSE) {
    bacdiveData <- .importBacDiveTable(
        verbose = verbose, forceDownload = forceDownload
    )
    colnames(bacdiveData) <- .changeBDColNames(colnames(bacdiveData))
    .getTidyBD(bacdiveData)
}

## Helper function for .getBacDive. The table is built from the BacDive API
## by sources/bacdive in waldronlab/bugphyzzExports (one row per strain, the
## layout of the former Google Sheet) and read through the same cache and
## base URL as the other programmatic sources.
.importBacDiveTable <- function(verbose = FALSE, forceDownload = FALSE) {
    if (verbose)
        message('Importing BacDive...')
    base <- .sourcesBaseUrl()
    tag <- substr(rlang::hash(base), 1, 8)
    fpath <- .getResource(
        paste0("sources_", tag, "_bacdive_bacdive.csv.gz"),
        paste0(base, "bacdive/bacdive.csv.gz"),
        force = forceDownload, verbose = FALSE
    )
    bacdive <- utils::read.csv(gzfile(fpath), stringsAsFactors = FALSE)
    colnames(bacdive) <- tolower(colnames(bacdive))
    return(bacdive)
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
.getTidyBD <- function(bacdiveData) {
    bacdiveData |>
        tidyr::pivot_longer(
            # Attributes start in the gram_stain column
            cols = gram_stain:tidyr::last_col(),
            names_to = 'Attribute', values_to = 'Attribute_value'
        ) |>
        dplyr::filter(Attribute_value != '') |>
        dplyr::mutate(Attribute = gsub('_', ' ', Attribute)) |>
        ## A strain can carry several values from different references (e.g.
        ## "anaerobe;facultative anaerobe"); keep every one as its own row.
        ## Only single-value attributes are split here: halophily and the
        ## metabolite columns parse ';' themselves, and the paired columns
        ## (culture temperature and its growth flag, biosafety level and its
        ## comment) hold one value per strain by construction.
        dplyr::mutate(
            Attribute_value = ifelse(
                Attribute %in% .multiValuedBDAttributes(),
                Attribute_value, gsub(';', '\u00a7', Attribute_value)
            )
        ) |>
        tidyr::separate_longer_delim(Attribute_value, delim = ';') |>
        dplyr::mutate(Attribute_value = gsub('\u00a7', ';', Attribute_value)) |>
        dplyr::mutate(Attribute_value = stringr::str_squish(Attribute_value)) |>
        dplyr::filter(Attribute_value != '') |>
        dplyr::mutate(
            Attribute = dplyr::case_when(
                Attribute == 'oxygen tolerance' ~ 'aerophilicity',
                Attribute == 'cell shape' ~ 'shape',
                Attribute == 'pathogenicity animal' ~ 'animal pathongen',
                Attribute == 'sample type' ~ 'isolation site',
                TRUE ~ Attribute
            )
        ) |>
        dplyr::distinct()
}

## Function for getting a list of data.frames (one per attribute)
## in tidy format from BacDive
.reshapeBacDive <- function(df) {

    df[['Attribute_source']] <- 'BacDive'
    splitDf <- split(df, factor(df[['Attribute']]))

    ## Attributes that must be changed from character to logical (simplest fix)
    attrNames <- c(
        'aerophilicity',
        'shape',
        'country',
        'cultivation medium used',
        'geographic location',
        'isolation site'
        ## colony color (delete)
    )
    
    ## Modifying an already existing vector rather than creating a list
    ## Keeping this for loop
    for (i in seq_along(attrNames)) {
        splitDf[[attrNames[i]]] <- .catToLog(splitDf[[attrNames[i]]])
        if (attrNames[i] %in% c('aerophilicity', 'shape')) {
            splitDf[[attrNames[i]]]$Attribute_type <-
                'multistate-intersection'
        } else {
            splitDf[[attrNames[i]]]$Attribute_type <- 'multistate-union'
        }
    }

    ## aerophilicity ####
    ## This is only to match the data in the bugphyzz spreadsheet
    aer <- splitDf[['aerophilicity']]
    aer$Attribute <- dplyr::case_when(
        aer$Attribute == 'aerobe' ~ 'aerobic',
        aer$Attribute == 'anaerobe' ~ 'anaerobic',
        aer$Attribute == 'facultative anaerobe' ~ 'facultatively anaerobic',
        aer$Attribute == 'microaerophile' ~ 'microaerophilic',
        aer$Attribute == 'obligate anaerobe' ~ 'obligately anaerobic',
        aer$Attribute == 'obligate aerobe' ~ 'obligately aerobic',
        TRUE ~ aer$Attribute
    )
    splitDf[['aerophilicity']] <- aer

    ## animal pathogen ####
    pos <- names(splitDf) == 'animal pathongen'
    names(splitDf)[pos] <- 'animal pathogen'
    x_ <- splitDf[['animal pathogen']][['Attribute_value']]
    x_ <- ifelse(x_ == "yes, in single cases", "yes", x_)
    x_ <- dplyr::case_when(x_ == 'yes' ~ TRUE, x_ == 'no' ~ FALSE)
    splitDf[['animal pathogen']][['Attribute_value']] <- x_
    splitDf[['animal pathogen']][['Attribute_group']] <- 'animal pathogen'
    splitDf[['animal pathogen']][['Attribute']] <- 'animal pathogen'
    splitDf[['animal pathogen']][['Attribute_type']] <- 'binary'

    ## biosafety level ####
    y <- splitDf[['biosafety level comment']][
        , c('BacDive_ID', 'Attribute_value')
    ]
    colnames(y)[2] <- 'Note'
    x <- dplyr::left_join(splitDf[['biosafety level']], y, by = 'BacDive_ID')
    x[['Attribute_value']] <- paste0('biosafety level ', x[['Attribute_value']])
    x[['Attribute']] <- x[['Attribute_value']]
    x[['Attribute_value']] <- TRUE
    x[['Attribute_group']] <- 'biosafety level'
    x[['Attribute_type']] <- 'multistate-intersection'
    splitDf[['biosafety level']] <- x
    splitDf[['biosafety level comment']] <- NULL

    ## colony color ####
    ## This one must be removed
    splitDf[['colony color']] <- NULL

    ## cultivation medium used - growth medium ####
    pos <- names(splitDf) == 'cultivation medium used'
    names(splitDf)[pos] <- 'growth medium'
    splitDf[['growth medium']][['Attribute_group']] <- 'growth medium'

    ## growth temperature ####
    ## culture temperature
    ## culture temperature growth
    ## culture temperature range (ignore)
    ## culture temperature type (ignore)
    splitDf[['culture temperature range']] <- NULL
    splitDf[['culture temperature type']] <- NULL
    a <- splitDf[['culture temperature']]
    b <- splitDf[['culture temperature growth']]
    b_ <- b[,c('BacDive_ID', 'Attribute_value')]
    colnames(b_)[2] <- 'growth'
    ab <- dplyr::left_join(a, b_, by = 'BacDive_ID')
    ab <- ab[ab[['growth']] == 'positive',]
    ab[['growth']] <- NULL
    ab[['Attribute_group']] <- 'growth temperature'
    ab[['Attribute_type']] <- 'range'
    ab[['Attribute']] <- 'growth temperature'
    splitDf[['growth temperature']] <- ab
    splitDf[['culture temperature']] <- NULL
    splitDf[['culture temperature growth']] <- NULL

    ## gram stain ####
    gs <- splitDf[['gram stain']]
    gs[['Attribute']] <- paste(gs[['Attribute']], gs[['Attribute_value']])
    gs[['Attribute_value']] <- TRUE
    gs[['Attribute_group']] <- 'gram stain'
    gs[['Attribute_type']] <- 'multistate-intersection'
    splitDf[['gram stain']] <- gs

    ## halophily ####
    validTerms <- c(
        'NaCl', 'KCl', 'MgCl2', 'MgCl2x6H2O', 'Na\\+', 'MgSO4x7H2O', 'Na2SO4',
        'Sea salts', 'Chromium \\(Cr6\\+\\)'
    )
    regex <- paste0('(', paste0(validTerms, collapse = '|'), ')')
    splitDf[['halophily']] <- splitDf[['halophily']] |>
        dplyr::mutate(Attribute_value = strsplit(Attribute_value, ';')) |>
        tidyr::unnest(cols = 'Attribute_value') |>
        dplyr::filter(!grepl('no growth', Attribute_value)) |>
        dplyr::mutate(
            Attribute_value = stringr::str_squish(Attribute_value),
            Attribute_value = sub('NaCL', 'NaCl', Attribute_value),
            Attribute_value = sub('Marine', 'Sea', Attribute_value),
            Attribute_value = sub('Salts', 'salts', Attribute_value)
        ) |>
        dplyr::filter(grepl(regex, Attribute_value)) |>
        dplyr::mutate(
            Attribute = stringr::str_extract(Attribute_value, regex),
            Unit = Attribute_value |>
                stringr::str_extract(' [<>]??[0-9]+\\.??[0-9]*.*') |>
                stringr::str_squish() |>
                stringr::str_remove('^.* '),
            Attribute_value = Attribute_value |>
                stringr::str_extract(' [<>]??[0-9]+\\.??[0-9]*.*') |>
                stringr::str_squish() |>
                stringr::str_remove(' .*$'),
            Attribute_group = 'halophily',
            Attribute_type = 'range'
        ) |>
        dplyr::filter(!grepl('[0-9]', Unit)) |>
        dplyr::distinct()

    ## hemolysis ####
    splitDf[['hemolysis']] <- splitDf[['hemolysis']] |>
        dplyr::mutate(
            Attribute_value = strsplit(Attribute_value, ';|/')
        ) |>
        tidyr::unnest(Attribute_value) |>
        dplyr::mutate(Attribute_value = stringr::str_squish(Attribute_value)) |>
        dplyr::filter(Attribute_value != '') |>
        dplyr::mutate(
            Attribute = Attribute_value,
            Attribute_value = TRUE,
            Attribute_group = 'hemolysis',
            Attribute_type = 'multistate-intersection'
        )

    ## incubation period
    ## This one must be removed
    splitDf[['incubation period']] <- NULL

    ## motility ####
    splitDf[['motility']] <- splitDf[['motility']] |>
        dplyr::mutate(
            Attribute_value = dplyr::case_when(
                Attribute_value == 'yes' ~ TRUE,
                Attribute_value == 'no' ~ FALSE
            )
        )
    splitDf[['motility']][['Attribute_group']] <- 'motility'
    splitDf[['motility']][['Attribute_type']] <- 'binary'

    ## pathogenicity human ####
    pat <- splitDf[['pathogenicity human']]
    pat[['Note']] <- stringr::str_extract(
        pat[['Attribute_value']], 'in single cases'
    )
    pat[['Note']] <- ifelse(is.na(pat[['Note']]), "", pat[['Note']])
    pat[['Attribute_value']] <- ifelse(
        grepl('^yes', pat[['Attribute_value']]), TRUE, NA
    )
    pat <- pat[!is.na(pat[['Attribute_value']]),]
    pat[['Attribute_group']] <- 'pathogenicity human'
    pat[['Attribute_type']] <- 'binary'
    splitDf[['pathogenicity human']] <- pat

    ## metabolite production ####
    mp <- splitDf[['metabolite production']]
    mp <- mp |>
        dplyr::mutate(Attribute_value = strsplit(Attribute_value, ';')) |>
        tidyr::unnest(Attribute_value)
    x <- stringr::str_extract(mp[['Attribute_value']], '(yes|no)$')
    mp <- mp[which(!is.na(x)),]
    y <- stringr::str_extract(mp[['Attribute_value']], '(yes|no)$')
    mp[['Attribute']] <- mp[['Attribute_value']]
    mp[['Attribute_value']] <- ifelse(y == 'yes', TRUE , FALSE)
    mp[['Attribute']] <- sub(' (yes|no)$', '', mp[['Attribute']])
    mp[['Attribute_group']] <- 'metabolite utilization'
    mp[['Attribute_type']] <- 'multistate-intersection'
    splitDf[['metabolite production']] <- mp

    ## metabolite utilization ####
    pos <- names(splitDf) == 'metabolite utiilization'
    names(splitDf)[pos] <- 'metabolite utilization'
    mu <- splitDf[['metabolite utilization']]
    mu <- mu |>
        dplyr::mutate(Attribute_value = strsplit(Attribute_value, ';')) |>
        tidyr::unnest(Attribute_value) |>
        dplyr::mutate(Attribute_value = stringr::str_squish(Attribute_value))
    x <- sub('^.* (\\+|-|\\+/-) *.*$', '\\1', mu[['Attribute_value']])
    y <- ifelse(!x %in% c('+', '-', '+/-'), NA, x)
    mu <- mu[which(!is.na(y)),]
    mu[['Attribute']] <- stringr::str_remove(
        mu[['Attribute_value']], ' (\\+|-|\\+/-) *.*$'
    )
    mu[['Note']] <- sub('^.*(\\+|-|\\+/-) ', '', mu[['Attribute_value']])
    mu[['Note']] <- paste('kind of utilization tested:', mu[['Note']])
    y <- y[!is.na(y)]
    mu[['Attribute_value']] <- dplyr::case_when(
        y == '+' ~ 'TRUE',
        y == '-' ~ 'FALSE',
        y == '+/-' ~ 'TRUE/FALSE'
    )
    mu <- mu |>
        dplyr::mutate(Attribute_value = strsplit(Attribute_value, '/')) |>
        tidyr::unnest(Attribute_value) |>
        dplyr::mutate(Attribute_value = as.logical(Attribute_value))
    mu[['Attribute_group']] <- 'metabolite utilization'
    mu[['Attribute_type']] <- 'multistate-intersection'
    splitDf[['metabolite utilization']] <- mu

    ## spore formation ####
    sf <- splitDf[['spore formation']]
    sf <- sf |>
        dplyr::mutate(
            Attribute_value = dplyr::case_when(
                Attribute_value == 'yes' ~ TRUE,
                Attribute_value == 'no' ~ FALSE
            ),
            Attribute_group = 'spore formation',
            Attribute_type = 'binary'
        ) |>
        dplyr::filter(!is.na(Attribute_value))
    splitDf[['spore formation']] <- sf

    splitDf <- lapply(splitDf, function(x) {
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
    })

    return(splitDf)
}

## Helper function for .reshapeBacDive
.catToLog <- function(df) {
    df[['Attribute_group']] <- df[['Attribute']]
    df[['Attribute']] <- df[['Attribute_value']]
    df[['Attribute_value']] <- TRUE
    df[['Attribute_type']] <- 'discrete'
    return(df)
}

## BacDive columns where several references can give different values
.multiValuedBDAttributes <- function() {
    c('oxygen tolerance', 'gram stain', 'cell shape', 'motility',
      'spore formation', 'hemolysis', 'sample type', 'country',
      'geographic location', 'cultivation medium used', 'colony color',
      'incubation period')
}
