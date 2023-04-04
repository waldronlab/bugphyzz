#' Import bugphyzz (categorical and binary)
#'
#' \code{importBugphyzz} imports bugphyzz as a single data.frame. It includes
#' attributes with categorical and binary (boolean) attributes.
#'
#' @param version Character string. The version to download. Default is 'devel'
#' (current file on the GitHub repo waldronlab/bugphyzzExports).
#' @param force_download Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#'
#' ## Check available groups of attributes
#' unique(bp$Attribute_group)
#'
#' ## Filter only for growth temperature
#' gt <- bp[which(bp$Attribute_group == 'growth temperature'), ]
#'
#' ## Create signatures with taxids at the species level
#' gt_sigs <- getBugphyzzSignatures(gt, tax.id.type = 'NCBI_ID', tax.level = 'species')]
#' lapply(gt_sigs, function(x) head(x))
#'
#'
importBugphyzz <- function(version = 'devel', force_download = FALSE
) {
  if (version == 'devel') {
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-exports/full_dump_categorical.csv.bz2'
  }
  rpath <- .getResource(
    rname = 'full_dump_categorical.csv.bz2', url = url, verbose = TRUE,
    force = force_download
  )
  ## TODO Add skip = 1 to the vroom call when header is added to the file
  bp <- vroom::vroom(
    file = rpath, show_col_types = FALSE, delim = ',', progress = FALSE,
    col_types = vroom::cols(NCBI_ID = vroom::col_character())
  )
  return(bp)
}

#' Import bugphyzz (numeric/continuous data)
#'
#' \code{importBugphyzzNumeric} imports a list of data.frames containing
#' numeric attributes. In this case, the attributes are in separate
#' data.frames because they can have different units or scales
#' (e.g. Celsius degrees, ph, etc.)
#'
#' @param version Character string. The version to download. Default is 'devel'
#' (current file on the GitHub repo waldronlab/bugphyzzExports).
#' @param force_download Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#'
#' @return A list of data.frames.
#' @export
#'
#' @examples
#'
#' bp_num <- importBugphyzzNumeric()
#'
#' ## Check available numeric attributes
#' names(bp_num)
#'
#' ## Select growth temperature
#' gt <- bp_num[['growth temperature']]
#'
#' ## Get taxa that grows better betwen 0 and 25 Celsius degrees
#' sub_gt <- gt[which(gt$Attribute_value_min >= 0 & gt$Attribute_value_max <= 25),]
#'
#' ## Creat signature at the genus level
#' sigs <- getBugphyzzSignatures(sub_gt, tax.id.type = 'Taxon_name', tax.level = 'genus')
#' head(sigs[[1]])
#'
importBugphyzzNumeric <- function(
    keyword = 'all', version = 'devel', force_download = FALSE
) {
  if (version == 'devel') {
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-exports/full_dump_numeric.csv.bz2'
  }
  rpath <- .getResource(
    rname = 'full_dump_numeric.csv.bz2', url = url, verbose = TRUE,
    force = force_download
  )
  ## TODO Add skip = 1 to the vroom call when header is added to the file
  bp <- vroom::vroom(
    file = rpath, show_col_types = FALSE, delim = ',', progress = FALSE,
    col_types = vroom::cols(NCBI_ID = vroom::col_character())
  )
  output <- split(bp, factor(bp$Attribute_group))
  return(output)
}

#' Get bugphyzz signatures
#'
#' \code{getBugphyzzSignatures} convert a data.frame imported with bugphyzz
#' to signatures.
#'
#' @param df A data.frame
#' @param tax.id.type A character string. NCBI_ID or Taxon_name.
#' @param tax.level A character string indicating taxonomic level.
#' Valid options: domain, phylum, class,
#' order, family, genus, species, strain.
#' @param evidence A character string indicating type of evidence.
#' Valid option: asr, inh, exp, igc, nas, tas. See details for meaning of
#' each keyword.
#' @param frequency A cahracter string indicating frequency.
#' Valid options: unknown, sometimes, usually, always. See details for
#' meaning of each keyword.
#' @param min.size An integer. Minimum number of elements in a signature.
#'
#' @return Named list of signatures.
#' @export
#'
#' @examples
#'
#' ## load purrr package (for managing lists)
#' library(purrr)
#'
#' ## Create helper function
#' sig_fun <- function(x) {
#'   getBugphyzzSignatures(
#'   df = x, tax.id.type = 'NCBI_ID', tax.level = 'species'
#'   )
#' }
#'
#' ## Create signatures of categorical or binary attributes
#' bp <- importBugphyzz()
#' sigs <- sig_fun(bp)
#' map(sigs, head)
#'
#' ## Create signatures of numeric attributes
#' bp_num <- importBugphyzzNumeric()
#' num_sigs <- flatten(map(bp_num, sig_fun))
#' lapply(num_sigs, head)
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
