
#' Import bugphyzz data as a single data.frame
#'
#' \code{importBugphyzz} imports bugphyzz.
#'
#' @param keyword The name of the physiology/attribute. Default is 'all'.
#' @param version Version to download. Default is 'devel'.
#' @param force_download Use cache or use a fresh download. Default is FALSE.
#'
#' @return A list of data.frames (tibbles).
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#'
importBugphyzz <- function(
    keyword = 'all', version = 'devel', force_download = FALSE
) {

  if (version == 'devel') {
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-exports/full_dump_categorical.csv.bz2'
  }

  rpath <- .getResource(
    rname = 'full_dump_categorical.csv.bz2', url = url, verbose = TRUE,
    force = force_download
  )
  # temp_dir <- tempdir()
  # temp_file <- paste0(temp_dir, '/', 'full_dump_categorical.csv.bz2')
  # download.file(url = url, destfile = temp_file, quiet = TRUE)
  ## Need to add skip = 1 to the vroom call when header is added to the text
  bp <- vroom::vroom(
    file = rpath, show_col_types = FALSE, delim = ',', progress = FALSE,
    col_types = vroom::cols(NCBI_ID = vroom::col_character())
  )
  if ('all' %in% keyword) {
    # output <- split(bp, factor(bp$Attribute_group))
    # output <- purrr::map(output, ~ purrr::discard(.x, ~all(is.na(.x))))
    return(bp)
  }
  bp <- bp |>
    dplyr::filter(.data$Attribute %in% keyword)
  # output <- split_df(bp, factor(bp$Attribute_group))
  # output <- purrr::map(output, ~ purrr::discard(.x, ~all(is.na(.x))))
  return(bp)
}

#' Import bugphyzz data as a list of numeric data.frames
#'
#' \code{importBugphyzzNumeric} imports numeric attributes as a list of
#' data.frames. Attributes are in separate data.frames since they can
#' have different units for filtering (e.g. Celsius degrees, ph, etc.)
#'
#' @param keyword Name of the numeric attributes.
#' Use \code{showBugphyzzNumeric} to check valid values.
#' Default value is 'all'.
#' @param version Zenodo version of dataset. If 'devel' the data from
#' the bugphyzzExports repo on GitHub will be downloaded.
#' @param force_download use cache or a fresh download. Default is FALSE.
#'
#' @return A list of data.frames.
#' @export
#'
#' @examples
#'
#' numeric <- importBugphyzzNumeric(version = 'devel', cache = TRUE)
#'
importBugphyzzNumeric <- function(
    keyword = 'all', version = 'devel', force_download = FALSE
) {
  if (version == 'devel') {
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-exports/full_dump_numeric.csv.bz2'
  }
  # temp_dir <- tempdir()
  # temp_file <- paste0(temp_dir, '/', 'full_dump_numeric.csv.bz2')
  # download.file(url = url, destfile = temp_file, quiet = TRUE)
  ## Need to add skip = 1 to the vroom call when header is added to the text

  rpath <- .getResource(
    rname = 'full_dump_numeric.csv.bz2', url = url, verbose = TRUE,
    force = force_download
  )
  bp <- vroom::vroom(
    file = rpath, show_col_types = FALSE, delim = ',', progress = FALSE,
    col_types = vroom::cols(NCBI_ID = vroom::col_character())
  )
  if ('all' %in% keyword) {
    output <- split(bp, factor(bp$Attribute_group))
    output <- purrr::map(output, ~ purrr::discard(.x, ~all(is.na(.x))))
    return(output)
  }
  bp <- bp |>
    dplyr::filter(.data$Attribute %in% keyword)
  output <- split_df(bp, factor(bp$Attribute_group))
  output <- purrr::map(output, ~ purrr::discard(.x, ~all(is.na(.x))))
  return(output)
}


#' Get Bugphyzz Signatures
#'
#' \code{getBugphyzzSignatures} convert data to signatures
#'
#' @param df A data.frame
#' @param tax.id.type NCBI_ID or Taxon_name.
#' @param tax.level Domain to strain.
#' @param evidence asr, inh, exp, igc, nas, tas
#' @param frequency unknown, sometimes, usually, always
#' @param min.size Default 5.
#'
#' @return List of signatures
#' @export
#'
#' @examples
#'
#' library(purrr)
#' bp <- importBugphyzz()
#' my_fun <- function(x) {
#'   getBugphyzzSignatures(
#'     df = x, tax.id.type = 'Taxon_name', tax.level = 'species', min.size = 5,
#'     frequency = 'always'
#'   )
#' }
#' o <- flatten(map(bp, my_fun))
#'
#'
getBugphyzzSignatures <- function(
    df, tax.id.type = 'NCBI_ID', tax.level = 'mixed',
    evidence = c('asr', 'inh', 'exp', 'tas', 'nas', 'igc'),
    frequency = c('unknown', 'always', 'usually', 'sometimes'),
    min.size = 5
) {
  valid_ranks <- c(
    'superkingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species',
    'strain'
  )
  if (tax.level == 'mixed') {
    tax.level <- valid_ranks
  }
  df <- df[which(df$Rank %in% tax.level),]
  df <- df[which(df$Evidence %in% evidence), ]
  df <- df[which(df$Frequency %in% frequency),]
  output <- split(df, factor(df$Attribute))
  output <- lapply(output, function(x) unique(x[[tax.id.type]]))
  output <- purrr::discard(output, ~ length(.x) < min.size)
  return(output)
}
