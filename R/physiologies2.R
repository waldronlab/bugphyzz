physiologies2 <- function(keyword = 'all') {

  if ('all' %in% keyword) {
    keyword <- showPhys()
  }

  links_df <- curationLinks()
  links_df <- links_df[links_df$physiology %in% keyword,]
  output <- vector('list', nrow(links_df))
  for (i in seq_along(output)) {
    one_row <- links_df[i, , drop = FALSE]
    phys_name <- one_row$physiology
    names(output)[i] <- phys_name
    output[[i]] <- .importPhysiology(one_row)
  }
  output
}

#' Import physiology
#'
#' \code{.importPhysiology} imports a physiology directly from one row of the
#' \code{curationLinks()} output.
#'
#' @param x A row from the output of \code{curationLinks()}.
#'
#' @keywords internal
#'
#' @return A data frame.
#'
.importPhysiology <- function(x) {

  link <- x$link
  attr_grp <- x$physiology
  attr_type <- x$sig_type

  message('Importing ', attr_grp, ' (', attr_type, ')')

  if (attr_type == 'categorical')
    df <- .importCategorical(link, attr_grp)

  if (attr_type == 'range')
    df <- .importRange(link, attr_grp)

  if (attr_type == 'numeric' || attr_type == 'logical')
    df <- read.csv(link)

  df$Attribute_type <- attr_type
  df$Attribute_group <- attr_grp

  df
}

.importCategorical <- function(link) {
  df <- read.csv(link)
  df
}

#' Import range
#'
#' \code{.importRange} imports a dataset labeled with "range" in
#' \code{curationLinks}.
#'
#' @param link A character string. Url to the spreadsheet.
#'
#' @return A data.frame.
#'
#' @keywords internal
#'
.importRange <- function(link) {
  link |>
    read.csv() |>
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
