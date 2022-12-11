
#' Import physiologies
#'
#' \code{physiologies2} imports physiologies from Google spreadsheets.
#'
#' @param keyword One or more values in a character vector. The values can
#' be checked with the \code{showPhys} function.
#'
#' @param remove_false if TRUE, remove all attributes with FALSE values.
#' Default is FALSE (no filtering).
#'
#' @return A list of data frames.
#' @export
#'
#' @examples
#'
#' phys <- physiologies2('all')
#' aer <- physiologies2('aerophilicity')
#'
#'
physiologies2 <- function(keyword = 'all', remove_false = FALSE) {

  ## Some checks about the validity of the keywords
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

  ## Import physiologies one by one
  links_df <- curationLinks()
  links_df <- links_df[links_df$physiology %in% keyword,]
  output <- vector('list', nrow(links_df))
  for (i in seq_along(output)) {
    one_row <- links_df[i, , drop = FALSE] # this ensures always a data.frame
    names(output)[i] <- one_row$physiology
    output[[i]] <- .importPhysiology(one_row, remove_false = remove_false)
  }
  output
}

#' Import physiology
#'
#' \code{.importPhysiology} imports a physiology directly from one row of the
#' \code{curationLinks()} output.
#'
#' @param x A row from the output of \code{curationLinks()}.
#' @param remvoe_false If TRUE, attributes with FALSE values are dropped.
#' Default is FALSE (all valeus included).
#'
#' @keywords internal
#'
#' @return A data frame.
#'
.importPhysiology <- function(x, remove_false = FALSE) {

  link <- x$link
  attr_grp <- x$physiology
  attr_type <- x$sig_type

  message('Importing ', attr_grp, ' (', attr_type, ')')
  df <- dplyr::distinct(utils::read.csv(link))
  df$NCBI_ID <- as.character(df$NCBI_ID) # This must be character always

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
    rp <- ranks_parents # ranks_parents is a data frame
    rp$NCBI_ID <- as.character(rp$NCBI_ID)
    rp$Parent_NCBI_ID <- as.character(rp$NCBI_ID)
    df <- dplyr::left_join(df, rp, by = "NCBI_ID")
  }

  ## Some general modification for the datasets
  df <- df |>
    purrr::modify_if(.p = is.character, ~ stringr::str_squish(.x)) |>
    .addConfidenceInCuration() |>
    purrr::modify_at(.at = c('Frequency', 'Evidence', 'Confidence_in_curation'), ~ {
      stringr::str_to_lower(.x)
    }) |>
    dplyr::distinct() |>
    .reorderColumns(attr_grp)

  ## Special modification for range values
  if (attr_type == 'range')
    df <- .modifyRange(df)

  ## Add some extra columns for attribute group (physiology) and
  ## type of signature (this will be relevant for creating signatures).
  df$Attribute_type <- attr_type
  df$Attribute_group <- attr_grp

  df
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
