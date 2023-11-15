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
importBugphyzz <- function(version = 'devel', force_download = FALSE) {
  if (version == 'devel')
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-workflow/bugphyzz_export_2023-11-12.tsv'
  rpath <- .getResource(
    rname = 'bugphyzz_export.tsv', url = url, verbose = TRUE,
    force = force_download
  )
  thr <- .thresholds()
  dat <- utils::read.table(rpath, header = TRUE, sep = '\t') |>
    dplyr::mutate(Attribute_source = ifelse(grepl('Asnicar F, Berry SE, Valdes AM, et al. Microbiome connections with host metabolism and habitual diet from 1,098 deeply phenotyped individuals. Nat Med. 2021;27(2):321-332. doi:10.1038/s41591-020-01183-1\\d', .data$Attribute_source), 'Asnicar_2021', .data$Attribute_source)) |>
    dplyr::mutate(Evidence = ifelse(grepl('Asnicar F, Berry SE, Valdes AM, et al. Microbiome connections with host metabolism and habitual diet from 1,098 deeply phenotyped individuals. Nat Med. 2021;27(2):321-332. doi:10.1038/s41591-020-01183-1\\d', .data$Attribute_source), 'igc', .data$Evidence))
  dplyr::left_join(dat, thr, by = c('Attribute_group', 'Attribute'))
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
  if (version == 'devel' || grepl("^[0-9a-z]{7}$", version)) {
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/main/full_dump_numeric.csv.bz2'
    ## update code when contente has been merged into main
    # if (version == 'devel') version <- 'main'
    # url <- paste0(
    #   'https://raw.githubusercontent.com/waldronlab/bugphyzzExports/', version,
    #   '/full_dump_numeric.csv.bz2'
    # )
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
    evidence = c('asr', 'inh', 'tax', 'inh2', 'exp', 'tas', 'nas', 'igc'),
    frequency = c('unknown', 'rarely', 'always', 'usually', 'sometimes'),
    min.size = 5
) {
  valid_ranks <- validRanks()
  if (tax.level == 'mixed') {
    tax.level <- valid_ranks
  }
  df <- df[which(df$Rank %in% tax.level),]
  df <- df[which(df$Evidence %in% evidence), ]
  df <- df[which(df$Frequency %in% frequency),]
  df$Attribute <- paste0(df$Attribute_group,'|', df$Attribute)
  df <- df |>
    dplyr::mutate(
      Attribute_range = ifelse(
        test = is.na(Attribute_range),
        yes = 'REMOVETHIS',
        no = Attribute_range)
    ) |>
    dplyr::mutate(
      Attribute = sub(
        ' REMOVETHIS$', '', paste0(Attribute, ' ', Attribute_range)
      )
    )
  dfs <- split(df, factor(df$Attribute))
  dfs <- lapply(dfs, function(x) unique(x[, c(tax.id.type, 'Rank')]))
  dfs <- purrr::discard(dfs, ~ nrow(.x) < min.size)
  sig_ranks <- purrr::map(dfs, ~ {
    v <- unique(.x$Rank)
    v <- factor(
      x = v, levels = c('domain', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'strain'),
      ordered = TRUE
    )
    v <- sort(v)
    v <- as.character(v)
    dplyr::case_when(
      v == 'domain' ~ 'd',
      v == 'kingdom' ~ 'k',
      v == 'phylum' ~ 'p',
      v == 'class' ~ 'c',
      v == 'order' ~ 'o',
      v == 'family' ~ 'f',
      v == 'genus' ~ 'g',
      v == 'species' ~ 's',
      v == 'strain' ~ 't',
      TRUE ~ v
    )
  })
  sig_ranks <- purrr::map_chr(sig_ranks, ~ paste0(.x, collapse = ''))
  sigs <- purrr::map(dfs, ~ unique(.x[[tax.id.type]]))
  names(sigs) <- paste0('bugphyzz:', names(sigs), '|', sig_ranks, recycle0 = TRUE)
  return(sigs)
}

#' Get Bug Annotations
#'
#' \code{getBugAnnotations} get all physiology annotations for one or more taxa.
#'
#' @param x A valid NCBI ID or taxon name
#' @param bp Import from \code{importBugphyzz}.
#' @param tax.id.type A character string. Either 'NCBI_ID' or 'Taxon_name'.
#'
#' @return A list of physiologies per taxa.
#' @export
#'
#' @examples
#'
#' x <- getBugAnnotations(
#'     x = c('561', '562'), bp = importBugphyzz(), tax.id.type = 'NCBI_ID'
#' )
#' x
#'
getBugAnnotations <- function(x, bp = importBugphyzz(), tax.id.type) {
  sub_bp <- bp[which(bp[[tax.id.type]] %in% x),]
  sub_bp |>
    { \(y) split(y, factor(y[[tax.id.type]])) }() |>
    purrr::map(~ split(.x, .x$Attribute_group)) |>
    purrr::map_depth(.depth = 2, ~ .x$Attribute) |>
    purrr::map_depth(.depth = 2, ~ sub('^.*:', '', .x))

}

#' Which Attributes
#'
#' \code{whichAttr} shows which attributes are present in a dataset imported
#' with \code{importBugphyzz}. This would be the names of the singatures
#' created with \code{getBugphyzzSignatures}.
#'
#' @param bp A data.frame imported with \code{importBugphyzz}.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#' whichAttr(bp)
#'
whichAttr <- function(bp) {
  sort(unique(bp$Attribute))
}

#' Which Attribute Groups
#'
#' \code{whichAttrGrp} shows which attribute groups are present in a dataset
#' imported with \code{importBugphyzz}.
#'
#' @param bp  A data.frame imported with \code{importBugphyzz}.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#' whichAttrGrp(bp)
#'
whichAttrGrp <- function(bp) {
  sort(unique(bp$Attribute_group))
}

#' Display taxonomic ranks
#'
#' \code{taxRanks} display the names of the taxonomic ranks used in bugphyzz.
#'
#' @return A character vector
#' @export
#'
#' @examples
#'
#' taxRanks()
#'
taxRanks <- function() {
  c(
    'strain', 'species', 'genus', 'family', 'order', 'class',
    'phylum', 'domain'
  )
}

#' Valid ranks
#'
#' \code{validRanks} returns valid ranks.
#'
#' @return A character vector.
#' @export
#'
validRanks <- function() {
  c(
    "kingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )
}

.thresholds <- function() {
  fpath <- file.path('extdata', 'thresholds.tsv')
  fname <- system.file(fpath, package = 'bugphyzz', mustWork = TRUE)
  utils::read.table(fname, header = TRUE, sep = '\t') |>
    dplyr::mutate(
      range = dplyr::case_when(
        is.na(.data$lower) ~ paste0('<=', .data$upper),
        is.na(.data$upper) ~ paste0('>=', .data$lower),
        TRUE ~ paste0(.data$lower, '-', .data$upper)
      ),
      unit = ifelse(is.na(.data$unit), '', .data$unit)
    ) |>
    dplyr::mutate(Attribute_range = paste0(range, unit)) |>
    dplyr::select(
      .data$Attribute_group, .data$Attribute, .data$Attribute_range
    )
}


