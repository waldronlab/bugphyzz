
#' Physiologies
#'
#' \code{physiologies} imports data from the
#' Google spreadsheets (https://drive.google.com/drive/folders/1i2UAolVWAYa7UnETNnCs0BDWjKPp3ev5).
#'
#' @param Character Character vector with one or more valid keywords.
#' Valid keyboard can be checked with \code{showPhys}.
#' @param full_source If TRUE, the full source is provided. If FALSE, a
#' shortened name of the source is provided. Default if FALSE.
#'
#' @return A list of data.frames in tidy format.
#'
#' @export
#'
#' @examples
#'
#' phys <- physiologies('all') # a list
#' df <- physiologies('aerophilicity')[[1]] # a data.frame
#'
physiologies <- function(keyword = 'all', full_source = FALSE) {
  keyword <- unique(sort(keyword))
  valid_keywords <- showPhys()
  if ('all' %in% keyword) {
    if (length(keyword) > 1)
      stop(
        "Found 'all' among the keywords. Are you sure that you want to ",
        "import all of the physiologies? If so, use 'all' alone.",
        call. = FALSE
      )
    keyword <- valid_keywords
  }
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

    if (unique(df$Attribute_group) == 'aerophilicity') {
      df <- .homogenizeAerophilicityAttributeNames(df)
    }

    dplyr::distinct(df)
  })

  return(physiologies)
}

#' Modify attributes of type range
#'
#' \code{.modifyRange} imports a dataset labeled with "range" in
#'
#' @param df A data frame.
#'
#' @return A data.frame.
#'
#' @keywords internal
#'
.modifyRange <- function(df) {
  num <- '[0-9]+(\\.[0-9]+)?'
  regex1 <- paste0('^\\-?', num, '(\\-', num, ')?$')
  regex2 <- paste0('^(<|>)(\\-)?', num, '$')
  regex <- paste0('(', regex1, '|', regex2, ')')
  df <- df |>
    dplyr::filter(grepl(regex, .data$Attribute_value)) |>
    dplyr::mutate(
      Attribute_value = sub('^(\\-)([0-9]+(\\.[0-9]+)?)', 'minus\\2', .data$Attribute_value)
    ) |>
    dplyr::mutate(
      Attribute_value = gsub(' ', '', .data$Attribute_value),
      Attribute_value = dplyr::case_when(
        grepl('<', .data$Attribute_value) ~ paste0('-', .data$Attribute_value),
        grepl('>', .data$Attribute_value) ~ paste0(.data$Attribute_value, '-'),
        !grepl("\\-", .data$Attribute_value) ~ paste0(.data$Attribute_value, '-', .data$Attribute_value),
        grepl("^\\-", .data$Attribute_value) ~ paste0("minusInf", .data$Attribute_value),
        grepl("\\-$", .data$Attribute_value) ~ paste0(.data$Attribute_value, "Inf"),
        TRUE ~ .data$Attribute_value
      ),
      Attribute_value = sub('(<|>)', '', .data$Attribute_value),
      Attribute_value = dplyr::case_when(
        grepl("^\\-", .data$Attribute_value) ~ paste0("minusInf", .data$Attribute_value),
        grepl("\\-$", .data$Attribute_value) ~ paste0(.data$Attribute_value, "Inf"),
        TRUE ~ .data$Attribute_value
      )
    ) |>
    tidyr::separate(
      col = 'Attribute_value',
      into = c('Attribute_value_min', 'Attribute_value_max'), sep = '-'
    ) |>
    dplyr::mutate(
        Attribute_value_min = sub('minus', '-', .data$Attribute_value_min),
        Attribute_value_max = sub('minus', '-', .data$Attribute_value_min)
      ) |>
    dplyr::mutate(
      Attribute_value_min = as.double(.data$Attribute_value_min),
      Attribute_value_max = as.double(.data$Attribute_value_max)
    ) |>
    dplyr::distinct()
}

#' Show list of available physiologies
#'
#' \code{showPhys} prints the names of the available physiologies that can be
#' imported with the \code{\link{physiologies}} function.
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
  fname <- system.file('extdata', 'links.tsv', package = 'bugphyzz')
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

## A function to reorder columns on import
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

.addSourceInfo <- function(df) {
  fpath <- system.file('extdata/attribute_sources.tsv', package = 'bugphyzz')
  data <- utils::read.table(
    file = fpath, header = TRUE, sep = '\t', quote = '', check.names = FALSE,
    comment.char = ''
  )
  dplyr::left_join(df, data, by = 'Attribute_source')
}

## This function imports the spreadsheets in extdata/links.tsv
.importSpreadsheets <- function(keyword) {
  parent_col_names <- c('Parent_name', 'Parent_NCBI_ID', 'Parent_rank')
  fname <- system.file('extdata', 'links.tsv', package = 'bugphyzz')
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
    } else if (unique(df[['Attribute_type']] %in% .DISCRETE_ATTRIBUTE_TYPES)) {
      df <- dplyr::filter(df, .data$Attribute_value == TRUE | .data$Attribute_value == FALSE)
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

## Helper function for making all numeric values into ranges
.numericToRange <- function(df) {
  df <- df |>
    dplyr::group_by(.data$NCBI_ID, .data$Taxon_name) |>
    dplyr::mutate(
      Attribute_value_min = as.double(.data$Attribute_value),
      Attribute_value_max = as.double(.data$Attribute_value),
      Attribute_type = 'range'
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct()
  df[['Attribute_value']] <- NULL
  return(df)
}

#' Homogenize attribute names of aerophilicity (helper for physiologies)
#'
#' \code{.homogenizeAerophilicityAttribbuteNames} makes all levels in the
#' aerophilicity dataset at the same level in the GO tree.
#'
#' @param df A data.frame.
#'
#' @return A data.frame.
#'
.homogenizeAerophilicityAttributeNames <- function(df) {
  df |> dplyr::mutate(
    Attribute = dplyr::case_when(
      .data$Attribute == 'obligately anaerobic' ~ 'anaerobic',
      .data$Attribute == 'microaerophilic' ~ 'aerobic',
      .data$Attribute == 'obligately aerobic' ~ 'aerobic',
      TRUE ~ .data$Attribute
    )
  )
}
