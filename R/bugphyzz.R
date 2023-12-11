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
    url <- 'https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/update-workflow/bugphyzz_export.tsv'
  rpath <- .getResource(
    rname = 'bugphyzz_export.tsv', url = url, verbose = TRUE,
    force = force_download
  )
  thr <- .thresholds()
  dat <- utils::read.table(rpath, header = TRUE, sep = '\t') |>
    dplyr::mutate(Score = round(.data$Score, digits = 3)) |>
    dplyr::mutate(Frequency = dplyr::case_when(
      .data$Score == 1 ~ 'always',
      .data$Score >= 0.9 & .data$Score < 1 ~ 'usually',
      .data$Score >= 0.5 & .data$Score < 0.9 ~ 'sometimes',
      .data$Score > 0 & .data$Score < 0.5 ~ 'rarely',
      .data$Score == 0  ~ 'never'
    )) |>
    dplyr::mutate(
      Attribute_source = ifelse(.data$Evidence == 'inh', NA, .data$Attribute_source)
    )
  dplyr::left_join(dat, thr, by = c('Attribute_group', 'Attribute'))
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
  valid_ranks <-   c(
    "kingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )
  if (tax.level == 'mixed') {
    tax.level <- valid_ranks
  }
  df <- df[which(df$Rank %in% tax.level),]
  df <- df[which(df$Evidence %in% evidence), ]
  df <- df[which(df$Frequency %in% frequency),]
  df$Attribute <- paste0(df$Attribute_group,'|', df$Attribute)

  if ('Attribute_range' %in% colnames(df)) {
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
  }

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
#' with \code{importBugphyzz}. Signatures created with
#' \code{getBugphyzzSignatures} would take these names.
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
