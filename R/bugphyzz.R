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
#' names(bp)
#'
importBugphyzz <- function(version = 'devel', force_download = FALSE) {
  types <- c("multistate", "binary", "numeric")
  urls <- paste0(
    "https://github.com/waldronlab/bugphyzzExports/raw/sdgamboa/phylo/bugphyzz_",
    types,
    ".csv"
  )
  names(urls) <- types
  if (version == 'devel') {
    output <- vector("list", length(urls))
    for (i in seq_along(output)) {
      message("Importing ", names(urls)[i], " data...")
      names(output)[i] <- names(urls)[i]
      rpath <- .getResource(
        rname = paste0("bugphyzz_", names(urls)[i], ".tsv"),
        url = urls[i], verbose = TRUE, force = force_download
      )
      output[[i]] <- utils::read.csv(rpath, header = TRUE, skip = 1)
    }
  }

  output <- lapply(output, function(x) split(x, x$Attribute_group))
  output <- purrr::list_flatten(output)
  names(output) <- purrr::map_chr(output, ~ unique(.x$Attribute_group))
  return(output)
}


#' Make bugs signatures
#'
#' \code{makeSignatures} Creates signatures for a list of bugphyzz
#' data.frames imported with \code{importBugphyzz}
#'
#' @param dat A data.frame.
#' @param tax_id_type A character string. Valid options: NCBI_ID, Taxon_name.
#' @param tax_level A character vector. Taxonomic rank. Valid options:
#' kingdom, phylum, class, order, family, genus, species, strain.
#' They can be combined. "mixed" is equivalent to select all valid ranks.
#' @param evidence A character vector. Valid options: exp, igc, nas, tas, tax, asr.
#' They can be combined. Default is all.
#' @param frequency A character vector. Valid options: always, usually,
#' sometimes, rarely, unknown. They can be combiend. Default value is all but
#' rarely.
#' @param min_size Minimun number of bugs in a signature. Default is 10.
#' @param min Minimum value inclusive. Only for numeric attributes. Default is NULL.
#' @param max Maximum value inclusive. Only for numeric attributes. Default is NULL.
#'
#' @return A list of character vector with the IDs of the bugs.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#' sigs <- lapply(bp, makeSignatures)
#' sigs <- purrr::list_flatten(sigs)
#'
makeSignatures <- function(
    dat, tax_id_type = "NCBI_ID",
    tax_level = "mixed",
    evidence = c("exp", "igc", "tas", "nas", "tax", "asr"),
    frequency = c("always", "usually", "sometimes", "unknown"),
    min_size = 10, min = NULL, max = NULL
) {
  attr_type <- unique(dat$Attribute_type)
  if (tax_level == "mixed") {
    tax_level <- c(
      "kingdom", "phylum", "class", "order", "family", "genus", "species",
      "strain"
    )
  }
  dat <- dat |>
    dplyr::filter(Rank %in% tax_level) |>
    dplyr::filter(.data$Evidence %in% evidence) |>
    dplyr::filter(.data$Frequency %in% frequency)
  if (!nrow(dat)) {
    warning(
      "Not enough data for creating signatures. Try different filtering options",
      call. = FALSE
    )
    return(NULL)
  }
  if (attr_type %in% c("multistate-intersection", "binary")) {
    s <- .makeSignaturesDiscrete(dat = dat, tax_id_type = tax_id_type)
  } else if (attr_type %in% c("range", "numeric")) {
    s <- .makeSignaturesNumeric(
      dat = dat, tax_id_type = tax_id_type, min = min, max = max
    )
  }
  output <- purrr::keep(s, ~ length(.x) >= min_size)
  if (!length(output)) {
    warning(
      "Not enough data for creating signatures. Try different filtering options",
      call. = FALSE
    )
  }
  return(output)
}

.makeSignaturesDiscrete <- function(dat, tax_id_type = "NCBI_ID") {
  dat |>
    dplyr::mutate(
      Attribute = paste0("bugphyzz:", .data$Attribute, "|", .data$Attribute_value)
    ) |>
    {\(y) split(y, y$Attribute)}() |>
    lapply(function(x) unique(x[[tax_id_type]]))
}

.makeSignaturesNumeric <- function(
    dat, tax_id_type = "NCBI_ID", min = NULL, max = NULL
) {
  if (!is.null(min) || !is.null(max)) {
    if (is.null(min)) {
      message("Minimum unespecified. Using ", min(dat$Attribute_value), ".")
      min <- min(dat$Attribute_value)
    }
    if (is.null(max)) {
      message("Maximum unespecified. Using ", max(dat$Attribute_value), ".")
      max <- max(dat$Attribute_value)
    }
    dat <- dat |>
      dplyr::filter(
        .data$Attribute_value >= min & .data$Attribute_value <= max
      ) |>
      dplyr::mutate(
        Attribute = paste0("bugphyzz:", .data$Attribute, "| >=", min, " & <=", max)
      )
  } else {
    thr <- .thresholds() |>
      dplyr::filter(.data$Attribute_group == unique(dat$Attribute_group))
    attr_name <- thr$Attribute
    min_values <- thr$lower
    max_values <- thr$upper
    dat$tmp_col <- NA
    for (i in seq_along(attr_name)) {
      if (is.na(min_values[i]))
        min_values[i] <- min(dat$Attribute_value) - 0.01
      if (is.na(max_values[i]))
        max_values[i] <- max(dat$Attribute_value)
      pos <- which(dat$Attribute_value > min_values[i] & dat$Attribute_value <= max_values[i])
      dat$tmp_col[pos] <- attr_name[i]
      dat$Attribute[pos] <- paste0("bugphyzz:", dat$Attribute[pos], "|", attr_name[i], "| > ", round(min_values[i], 2), " & <= ", max_values[i])
    }
  }
  dat |>
    {\(y) split(y, y$Attribute)}() |>
    lapply(function(x) unique(x[[tax_id_type]]))
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
    dplyr::relocate(
      .data$Attribute_group, .data$Attribute, .data$Attribute_range
    )
}
