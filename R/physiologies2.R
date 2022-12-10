
physiologies2 <- function(keyword = 'all') {
  links_df <- curationLinks()
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

  if (attr_type == 'logical')
    df <- .importLogical(link, attr_grp)

  if (attr_type == 'range')
    df <- .importRange(link, attr_grp)

  if (attr_type == 'numeric')
    df <- .importNumeric(link, attr_grp)

  df$Attribute_type <- attr_type
  df$Attribute_group <- attr_grp

  df
}

.importCategorical <- function(link, phys_name) {

  df <- read.csv(link)
}

.importLogical <- function(link, phys_name) {
  df <- read.csv(link)

}
.importRange <- function(link, phys_name) {
  df <- read.csv(link)

}

.importNumeric <- function(link, phys_name) {
  df <- read.csv(link)
}
