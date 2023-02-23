
#' Import physiologies
#'
#' \code{physiologies} imports physiologies from Google spreadsheets.
#'
#' @param keyword One or more values in a character vector. The values can
#' be checked with the \code{showPhys} function.
#' @param remove_false if TRUE, remove all attributes with FALSE values.
#' Default is FALSE (no filtering).
#' @param full_source If TRUE, full source is provided. If FALSE, a
#' shortened name of the source.
#'
#' @return A list of data frames.
#' @export
#'
#' @examples
#'
#' phys <- physiologies('all')
#' aer <- physiologies('aerophilicity')
#'
#'
physiologies <- function(
    keyword = 'all', remove_false = FALSE, full_source = TRUE
) {
  keyword <- unique(keyword)
  valid_keywords <- showPhys()
  if ('all' %in% keyword) {
    if (length(keyword) > 1)
      message("Found 'all' among the keywords. Importing all physiologies.")
    keyword <- valid_keywords
  }
  lgl_vct <- keyword %in% valid_keywords
  if (any(!lgl_vct)) {
    invalid_keywords <- keyword[!lgl_vct]
    stop(
      "Invalid keyword(s): ", paste0(invalid_keywords, collapse = ', '), '.',
      " Check valid options with showPhys().",
      call. = FALSE
    )
  }
  links_df <- curationLinks()
  links_df <- links_df[links_df$physiology %in% keyword,]
  output <- vector('list', nrow(links_df))
  for (i in seq_along(output)) {
    one_row <- links_df[i, , drop = FALSE]
    names(output)[i] <- one_row$physiology
    output[[i]] <- .importPhysiology(
      one_row, remove_false = remove_false, full_source = full_source
    )
  }
  return(output)
}

#' Import physiology
#'
#' \code{.importPhysiology} imports a physiology directly from one row of the
#' \code{curationLinks()} output.
#'
#' @param x A row from the output of \code{curationLinks()}.
#' @param remvoe_false If TRUE, attributes with FALSE values are dropped.
#' Default is FALSE (all values included).
#' @param full_source if TRUE, the full source is displayed. Otherwise, an
#' abreviated form.
#'
#' @keywords internal
#'
#' @return A data frame.
#'
.importPhysiology <- function(x, remove_false = FALSE, full_source) {

  link <- x$link
  attr_grp <- x$physiology
  attr_type <- x$sig_type

  message('Importing ', attr_grp, ' (', attr_type, ')')
  df <- dplyr::distinct(utils::read.csv(link))
  df$NCBI_ID <- as.character(df$NCBI_ID)

  if (remove_false)
    df <- dplyr::filter(df, !Attribute_value == FALSE)

  ## Remove missing data from the Attribute_value column
  nmissing <- sum(is.na(df$Attribute_value))
  if (nmissing > 0) {
    message(
      "Dropped ", nmissing, " rows with missing Attribute_value from ",
      attr_grp
    )
  }else{
    message("Finished ", attr_grp)
  }
  df <- df[!is.na(df$Attribute_value), ]

  ## Add rank and parent information for taxa with NCBI_ID
  col_names <- colnames(df)
  parent_col_names <- c('Parent_name', 'Parent_NCBI_ID', 'Parent_rank')
  if (all(parent_col_names %in% col_names)) {
    df$Parent_NCBI_ID <- as.character(df$Parent_NCBI_ID)
  } else {
    rp <- ranks_parents # ranks_parents is a data.frame object in bugphyzz
    rp$NCBI_ID <- as.character(rp$NCBI_ID)
    rp$Parent_NCBI_ID <- as.character(rp$Parent_NCBI_ID)
    df <- dplyr::left_join(df, rp, by = "NCBI_ID")
  }

  ## Some general modification for the datasets
  df <- df |>
    purrr::modify_if(.p = is.character, ~ stringr::str_squish(.x)) |>
    .addSourceInfo() |>
    purrr::modify_at(
      .at = c('Frequency', 'Evidence', 'Confidence_in_curation'),
      ~ stringr::str_to_lower(.x)
    ) |>
    dplyr::distinct()

  ## Special modification for range values
  if (attr_type == 'range')
    df <- .modifyRange(df)

  df <- .reorderColumns(df,name = attr_grp, attr_type = attr_type)

  ## Add some extra columns for attribute group (physiology) and
  ## type of signature (this will be relevant for creating signatures).
  df$Attribute_type <- attr_type
  df$Attribute_group <- attr_grp

  ## Change source if needed
  if (full_source) {
    df$Attribute_source <- df$full_source
  }
  df$full_source <- NULL
  return(df)
}

#' Import range
#'
#' \code{.modifyRange} imports a dataset labeled with "range" in
#' \code{curationLinks}.
#'
#' @param df A data frame.
#'
#' @return A data.frame.
#'
#' @keywords internal
#'
.modifyRange <- function(df) {
  ## Some lines are beyond the recommended 80 characters length, but I think is
  ## better for readebiliy of the code.
  df |>
    dplyr::mutate(
      Attribute_value = gsub(' ', '', .data$Attribute_value),
      Attribute_value = dplyr::case_when(
        grepl('<', .data$Attribute_value) ~ paste0('-', .data$Attribute_value),
        grepl('>', .data$Attribute_value) ~ paste0(.data$Attribute_value, '-'),
        !grepl("-", .data$Attribute_value) ~ paste0(.data$Attribute_value, '-', .data$Attribute_value),
        grepl("^-", .data$Attribute_value) ~ paste0("0", .data$Attribute_value),
        grepl("-$", .data$Attribute_value) ~ paste0(.data$Attribute_value, "Inf"),
        TRUE ~ .data$Attribute_value
      ),
      Attribute_value = sub('(<|>)', '', .data$Attribute_value),
      Attribute_value = dplyr::case_when(
        ## For some reason this does not work int he case_when call above. ??
        grepl("^-", .data$Attribute_value) ~ paste0("0", .data$Attribute_value),
        grepl("-$", .data$Attribute_value) ~ paste0(.data$Attribute_value, "Inf"),
        TRUE ~ .data$Attribute_value
      )
    ) |>
    tidyr::separate(
      col = 'Attribute_value',
      into = c('Attribute_value_min', 'Attribute_value_max'), sep = '-'
    ) |>
    dplyr::mutate(
      Attribute_value_min = as.double(.data$Attribute_value_min),
      Attribute_value_max = as.double(.data$Attribute_value_max)
    )
}

#' Show links to curation spreadsheets
#'
#' @param keyword a character vector of physiologies desired. For the available
#' physiologies, run bugphyzz::showPhys(). Use "all" for all available physiologies.
#'
#' @return a data.frame with physiology names and URLs
#' @keywords internal
#'
#' @examples
#' bugphyzz:::curationLinks()
#' bugphyzz:::curationLinks(keyword = "aerophilicity")
#' bugphyzz:::curationLinks(keyword = c("aerophilicity", "gram stain"))
curationLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "links.tsv"), package = "bugphyzz")
  links <- utils::read.table(fname, sep = "\t", header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}
#' List of available physiologies
#'
#' \code{showPhys} prints the names of the available datasets provided by
#' the \code{\link{physiologies}} function.
#'
#' @return A character vector with the names of the physiologies datasets.
#' @export
#'
#' @examples
#' x <- showPhys()
showPhys <- function(){
  curationLinks()[["physiology"]]
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
