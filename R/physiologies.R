
#' Import physiologies (for devs)
#'
#' \code{physiologies} imports a list of data.frames. This data is in "raw"
#' state before cleaning and going through the data imputation steps. It
#' should be used by developers/curators of the package.
#'
#' @param keyword Character vector with one or more valid keywords.
#' Valid keyboards can be checked with \code{showPhys}. If 'all', all
#' physiologies are imported.
#' @param full_source Logical. If `TRUE`, the Attribute_source column will
#' contain full source information. If `FALSE`, the Attribute_source column will
#' contain shortened versions of the sources. Default is `FALSE`.
#'
#' @return A list of data.frames in tidy format.
#'
#' @export
#'
#' @examples
#' l <- physiologies('all')
#' df <- physiologies('aerophilicity')[[1]]
#'
physiologies <- function(keyword = 'all', full_source = FALSE) {
  keyword <- .checkKeyword(keyword)
  cond1 <- any(keyword %in% showPhys('spreadsheets'))
  cond2 <- any(keyword %in% showPhys('bacdive'))

  if (cond1 && cond2) {
    spreadsheets <- .importSpreadsheets(keyword = keyword)
    spreadsheets <- spreadsheets[names(spreadsheets) %in% keyword]
    bacdive <- .reshapeBacDive(.getBacDive(verbose = FALSE))
    bacdive <- bacdive[names(bacdive) %in% keyword]
    physiologies <- vector('list', length(keyword))
    for (i in seq_along(keyword)) {
        df1 <- spreadsheets[[keyword[i]]]
        df2 <- bacdive[[keyword[i]]]
        physiologies[[i]] <- dplyr::bind_rows(df1, df2)
        names(physiologies)[i] <- keyword[i]
        message('Finished ', keyword[i], '.')
    }
  } else if (cond1 && !cond2) {
    spreadsheets <- .importSpreadsheets(keyword = keyword)
    physiologies <- spreadsheets[names(spreadsheets) %in% keyword]
    for (i in seq_along(keyword)) {
      message('Finished ', keyword[i], '.')
    }
  } else if (!cond1 && cond2) {
    bacdive <- .reshapeBacDive(.getBacDive(verbose = FALSE))
    physiologies <- bacdive[names(bacdive) %in% keyword]
    for (i in seq_along(keyword)) {
      message('Finished ', keyword[i], '.')
    }
  }

  physiologies <- lapply(physiologies, function(df) {
    df <- df |>
      purrr::modify_if(.p = is.character, ~ stringr::str_squish(.x)) |>
      .addSourceInfo() |>
      purrr::modify_at(
        .at = c('Frequency', 'Evidence', 'Confidence_in_curation'),
        ~ stringr::str_squish(stringr::str_to_lower(.x))
      ) |>
      dplyr::distinct()

    if (full_source) {
      df$Attribute_source <- df$full_source
    }
    df$full_source <- NULL

    df <- .reorderColumns(
      df = df,
      name = unique(df$Attribute_group),
      attr_type = unique(df$Attribute_type)
    )

    df <- as.data.frame(df[, vapply(df, \(y) !all(is.na(y)), logical(1))])

    ## TODO this code could be somewhere else
    if (unique(df$Attribute_group) == 'aerophilicity') {
      df <- .homogenizeAerophilicityAttributeNames(df)
    }

    dplyr::distinct(df)
  })

  return(physiologies)
}

#' Show list of available physiologies (for devs)
#'
#' \code{showPhys} prints the names of the available physiologies that can be
#' imported with the \code{\link{physiologies}} function. This function
#' should be used by developers/curators.
#'
#' @param which_names A character string. Options: 'all' (default),
#' 'spreadsheets', 'bacdive'.
#'
#' @return A character vector with the names of the physiologies.
#' @export
#'
#' @examples
#' showPhys()
#' showPhys('bacdive')
#' showPhys('spreadsheets')
#'
showPhys <- function(which_names = 'all') {
  fname <- system.file('extdata', 'spreadsheet_links.tsv', package = 'bugphyzz')
  links <- utils::read.table(fname, header = TRUE, sep = '\t')
  spreadsheet_phys <- links[['physiology']]
  if (which_names == 'all')
    ## bacdive_phys_names is a character vector saved as internal data
    phys_names <- sort(unique(c(spreadsheet_phys, bacdive_phys_names)))
  if (which_names == 'spreadsheets')
    phys_names <- spreadsheet_phys
  if (which_names == 'bacdive')
    phys_names <- bacdive_phys_names
  return(phys_names)
}

## Helper function for physiologies
.checkKeyword <- function(keyword) {
  keyword <- unique(sort(keyword))
  if ('all' %in% keyword) {
    if (length(keyword) > 1) {
      stop(
        "Found 'all' among the keywords. Are you sure that you want to ",
        "import all of the physiologies? If so, use 'all' alone. Quitting.",
        call. = FALSE
      )
    } else if (length(keyword) == 1) {
      message('All physiologies will be imported.')
      keyword <- showPhys()
    }
  }
  valid_keywords <- showPhys()
  lgl_vct <- keyword %in% valid_keywords
  if (any(!lgl_vct) ) {
    invalid_keywords <- keyword[!lgl_vct]
    stop(
      "Invalid keyword(s): ", paste0(invalid_keywords, collapse = ', '), '.',
      " Check valid keywords with showPhys() or use 'all' to import all",
      " physiologies.",
      call. = FALSE
    )
  }
  return(keyword)
}

## Helper function for physiologies
.importSpreadsheets <- function(keyword) {
  parent_col_names <- c('Parent_name', 'Parent_NCBI_ID', 'Parent_rank')
  fname <- system.file('extdata', 'spreadsheet_links.tsv', package = 'bugphyzz')
  links <- utils::read.table(fname, header = TRUE, sep = '\t')
  links <- links[links[['physiology']] %in% keyword,]
  spreadsheets <- vector('list', nrow(links))
  for (i in seq_along(spreadsheets)) {
    phys_name <- links[i, 'physiology', drop = FALSE][[1]]
    attr_type <- links[i, 'attribute_type', drop = FALSE][[1]]
    names(spreadsheets)[i] <- phys_name
    url <- links[i, 'link', drop = FALSE][[1]]
    df <- dplyr::distinct(utils::read.csv(url))
    df[['Attribute_type']] <- attr_type
    df[['Attribute_group']] <- phys_name
    df[['NCBI_ID']] <- as.character(df[['NCBI_ID']])
    df <- df[!is.na(df[['Attribute_value']]),]

    if (unique(df[['Attribute_type']]) == 'numeric') {
      df <- .numericToRange(df)
    } else if (unique(df[['Attribute_type']] == 'range')) {
      df <- .modifyRange(df)
    } else if (unique(df[['Attribute_type']] %in% .DISCRETE_ATTRIBUTE_TYPES())) {
      df <- dplyr::filter(df, Attribute_value == TRUE | Attribute_value == FALSE)
    }

    if (all(parent_col_names %in% colnames(df))) {
      df$Parent_NCBI_ID <- stringr::str_squish(as.character(df$Parent_NCBI_ID))
    } else {
        ## ranks_parents is an internal object (data.frame) in bugphyzz
        rp <- purrr::modify_at(
          .x = ranks_parents,
          .at = c('NCBI_ID', 'Parent_NCBI_ID'),
          .f = as.character
        )
        df <- dplyr::left_join(df, rp, by = "NCBI_ID")
    }
    spreadsheets[[i]] <- df
  }
  return(spreadsheets)
}

## Helper function for .importSpreadsheets
.numericToRange <- function(df) {
  df <- df |>
    dplyr::group_by(NCBI_ID, Taxon_name) |>
    dplyr::mutate(
      Attribute_value_min = as.double(Attribute_value),
      Attribute_value_max = as.double(Attribute_value),
      Attribute_type = 'range'
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  df[['Attribute_value']] <- NULL
  return(df)
}

## Helper function for .importSpreadsheets
.modifyRange <- function(df) {
  num <- '[0-9]+(\\.[0-9]+)?'
  regex1 <- paste0('^\\-?', num, '(\\-', num, ')?$')
  regex2 <- paste0('^(<|>)(\\-)?', num, '$')
  regex <- paste0('(', regex1, '|', regex2, ')')
  df <- df |>
    dplyr::filter(grepl(regex, Attribute_value)) |>
    dplyr::mutate(
      Attribute_value = sub('^(\\-)([0-9]+(\\.[0-9]+)?)', 'minus\\2', Attribute_value)
    ) |>
    dplyr::mutate(
      Attribute_value = gsub(' ', '', Attribute_value),
      Attribute_value = dplyr::case_when(
        grepl('<', Attribute_value) ~ paste0('-', Attribute_value),
        grepl('>', Attribute_value) ~ paste0(Attribute_value, '-'),
        !grepl("\\-", Attribute_value) ~ paste0(Attribute_value, '-', Attribute_value),
        grepl("^\\-", Attribute_value) ~ paste0("minusInf", Attribute_value),
        grepl("\\-$", Attribute_value) ~ paste0(Attribute_value, "Inf"),
        TRUE ~ Attribute_value
      ),
      Attribute_value = sub('(<|>)', '', Attribute_value),
      Attribute_value = dplyr::case_when(
        grepl("^\\-", Attribute_value) ~ paste0("minusInf", Attribute_value),
        grepl("\\-$", Attribute_value) ~ paste0(Attribute_value, "Inf"),
        TRUE ~ Attribute_value
      )
    ) |>
    tidyr::separate(
      col = 'Attribute_value',
      into = c('Attribute_value_min', 'Attribute_value_max'), sep = '-'
    ) |>
    dplyr::mutate(
      Attribute_value_min = sub('minus', '-', Attribute_value_min),
      Attribute_value_max = sub('minus', '-', Attribute_value_min)
    ) |>
    dplyr::mutate(
      Attribute_value_min = as.double(Attribute_value_min),
      Attribute_value_max = as.double(Attribute_value_max)
    ) |>
    dplyr::distinct()
}

## helper function for .importSpreadsheets
.DISCRETE_ATTRIBUTE_TYPES <- function() {
  fname <- system.file('extdata', 'spreadsheet_links.tsv', package = 'bugphyzz')
  dat <- utils::read.table(file = fname, header = TRUE, sep = '\t')
  unique(dat[dat$trait_type == 'discrete',]$attribute_type)
}

## Helper function for physiologies
.addSourceInfo <- function(dat) {
  fpath <- system.file('extdata', 'attribute_sources.tsv', package = 'bugphyzz')
  source_data <- utils::read.table(
    file = fpath, header = TRUE, sep = '\t', quote = '', check.names = FALSE,
    comment.char = ''
  )
  dplyr::left_join(dat, source_data, by = 'Attribute_source')
}

## Helper function for physiologies
.reorderColumns <- function(df, name = NULL, attr_type) {
  col_names <- colnames(df)
  req_cols <- .requiredColumns(attr_type)
  cols_lgl <- req_cols %in% col_names
  if (!all(cols_lgl)) {
    missing_cols <- paste0(req_cols[!cols_lgl], collapse = ', ')
    if (!is.null(name)) {
      msg <- paste0(
        'Missing columns in ', name, '.', ' Missing columns are: ',
        missing_cols
      )
    } else {
      msg <- paste0(
        'Missing columns.', ' Missing columns are: ', missing_cols
      )
    }
    warning(msg, call. = FALSE)
  }
  cols <- req_cols[cols_lgl]
  df |>
    dplyr::relocate(dplyr::all_of(cols))
}

## Helper function for physiologies
.homogenizeAerophilicityAttributeNames <- function(df) {
  df |> dplyr::mutate(
    Attribute = dplyr::case_when(
      Attribute == 'obligately anaerobic' ~ 'anaerobic',
      Attribute == 'microaerophilic' ~ 'aerobic',
      Attribute == 'obligately aerobic' ~ 'aerobic',
      TRUE ~ Attribute
    )
  )
}

## Required columns
.requiredColumns <- function(attr_type) {
  fname <- system.file("extdata/curation_template.tsv", package = "bugphyzz")
  df <- utils::read.table(fname, sep = "\t", header = TRUE)
  lgl_vct_1 <- df$requiredness == "required"
  lgl_vct_2 <- grepl(attr_type, df$attribute_types)
  df <- df[lgl_vct_1 & lgl_vct_2,]
  df[order(df[["required_column_order"]]), , drop = FALSE]
  output <- df[['column_name']]
  return(output)
}

## Generate a template for a bugphyzz dataset
.template <- function(dataset) {
  template_tsv <- system.file("extdata/curation_template.tsv", package = "bugphyzz")
  template <- utils::read.table(
    file = template_tsv, sep = "\t", check.names = FALSE, header = TRUE,
    allowEscapes = TRUE )
  # template <- readr::read_tsv(template_tsv, show_col_types = FALSE)
  template[template[["column_name"]] %in% colnames(dataset), ]
}

## Print valid attributes
.attributes <- function() {
  fname <- system.file("extdata/attributes.tsv", package = "bugphyzz")
  df <- utils::read.table(
    fname, sep = "\t", header = TRUE, check.names = FALSE
  )
  unique(df[,"attribute"])
}

## Append links to error table
.appendLinks <- function(x) {
  fname1 <- system.file('extdata/spreadsheet_links.tsv', package = 'bugphyzz')
  links <- utils::read.table(fname1, header = TRUE, sep = '\t')
  select_cols <- c("physiology", "source_link")
  phys_links <- links |>
    dplyr::select(tidyselect::all_of(select_cols))
  custom_links <- .customLinks() |>
    dplyr::select(tidyselect::all_of((select_cols)))
  links <- dplyr::bind_rows(phys_links, custom_links)
  x |>
    dplyr::left_join(links, by = c("dataset" = "physiology"))
}

