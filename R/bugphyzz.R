
#' Import bugphyzz as a single data.frame (or several?
#'
#' \code{importBugphyzz} imports bugphyzz.
#'
#' @param keyword The name of the physiology/attribute. Default is 'all'.
#' @param version Version to download. Default is 'devel'.
#' @param cache Use cache or use a fresh download.
#'
#' @return A list of data.frames (tibbles).
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#'
importBugphyzz <- function(keyword = 'all', version = 'devel', cache = TRUE) {
  if (cache) {
    ## TODO add code for BiocFileCache
  }
  if (version == 'devel') {
    # url <- 'https://github.com/waldronlab/bugphyzzExports/blob/sdgamboa/update-exports/full_dump_bugphyzz.csv.bz2?raw=true'
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-exports/full_dump_bugphyzz.csv.bz2'
  }
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, '/', 'full_dump_bugphyzz.csv.bz2')
  download.file(url = url, destfile = temp_file, quiet = TRUE)
  ## Need to add skip = 1 to the vroom call when header is added to the text
  bp <- vroom::vroom(
    file = temp_file, show_col_types = FALSE, delim = ',', progress = FALSE,
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
