#' Import bugphyzz
#'
#' \code{importBugphyzz} imports bugphyzz as a single data.frame. It includes
#' attributes with categorical and binary (boolean) attributes.
#'
#' @param version Character string. The version to download. Default is 'devel'
#' (current file on the GitHub repo waldronlab/bugphyzzExports).
#' @param force_download Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#' @param v Validation value. Default 0.5.
#' @param remove_asr Logical. Default is TRUE.
#' @param exclude_rarely Default is TRUE.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#' names(bp)
#'
importBugphyzz <- function(
    version = 'devel', force_download = FALSE, v = 0.5, exclude_rarely = TRUE

) {
  types <- c("multistate", "binary", "numeric")
  urls <- paste0(
    "https://github.com/waldronlab/bugphyzzExports/raw/main/bugphyzz_",
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
      output[[i]] <- utils::read.csv(rpath, header = TRUE, skip = 1) |>
        dplyr::mutate(Attribute = tolower(.data$Attribute))
    }
  }
  output <- lapply(output, function(x) split(x, x$Attribute))
  output <- purrr::list_flatten(output)

  ## TODO correct plant pathogenicity name earlier in the workflow or
  ## better yet, directly in the curation
  pos <- which(names(output) == "plant pathogenity")
  names(output)[pos] <- "plant pathogenicity"
  output <- purrr::map(output, ~ {
    .x |>
      dplyr::mutate(
        Attribute = ifelse(.data$Attribute == "plant pathogenity", "plant pathogenicity", .data$Attribute)
      )
  })

  names(output) <- purrr::map_chr(output, ~ unique(.x$Attribute))
  val <- .validationData() |>
    dplyr::filter(.data$rank == "all") |>
    dplyr::select(.data$physiology, .data$attribute, .data$value) |>
    dplyr::mutate(physiology = tolower(.data$physiology)) |>
    dplyr::mutate(attribute = tolower(.data$attribute))

  output <- purrr::map(output, ~ {
    attr_type <- unique(.x$Attribute_type)
    if (attr_type == "binary") {
      val <- dplyr::select(val, Attribute = .data$attribute, .data$value)
      o <- dplyr::left_join(.x, val, by = "Attribute" )
    } else if (attr_type == "multistate-intersection" || attr_type == "multistate-union") {
      val <- dplyr::select(val, Attribute = .data$physiology, Attribute_value = .data$attribute, .data$value)
      o <- dplyr::left_join(mutate(.x, Attribute_value = tolower(.data$Attribute_value)) , val, by = c("Attribute", "Attribute_value"))
    } else if (attr_type == "numeric") {
      val <- dplyr::select(val, Attribute = .data$attribute, .data$value)
      o <- dplyr::left_join(.x, val, by = "Attribute") |>
        dplyr::rename(NSTI = .data$nsti)
    }
    o |>
      dplyr::filter(
       !(.data$value < v & .data$Evidence == "asr")
      ) |>
      dplyr::mutate(value = ifelse(.data$Evidence != "asr", NA, value)) |>
      dplyr::rename(Validation = .data$value)
  })

  if (exclude_rarely) {
    output <- purrr::map(output, ~ dplyr::filter(.x, .data$Frequency != "rarely"))
  }
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
  if ("mixed" %in% tax_level) {
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
  if (attr_type %in% c("multistate-intersection", "binary", "multistate-union")) {
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

#' Get Taxon Signatures
#'
#' \code{getTaxonSignatures} get the names of all of the signatures for a taxon.
#'
#' @param tax A valid NCBI ID or taxon name. If taxon name is used, the
#' tax_id_type = "Taxon_name" must also be used.
#' @param bp Import from \code{importBugphyzz}.
#' @param ... Arguments passed to \code{makeSignatures}.
#'
#' @return A character vector with the names of the signatures for a taxon.
#' @export
#'
#' @examples
#' taxid <- "562"
#' bp <- importBugphyzz()
#' sig_names_1 <- getTaxonSignatures("562", bp)
#' sig_names_2 <- getTaxonSignatures("Escherichia coli", bp, tax_id_type = "Taxon_name")
#'
getTaxonSignatures <- function(tax, bp, ...) {
  sigs <- purrr::map(bp, makeSignatures, ...)
  sigs <- purrr::list_flatten(sigs, name_spec = "{inner}")
  pos <- which(purrr::map_lgl(sigs, ~ tax %in% .x))
  output <- names(sigs)[pos]
  return(output)
}



#' Import validation data
#'
#' \code{importValidation} impots the result of the 10-fold cross-validation.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' val <- importValidation()
#'
importVal <- function() {
  url <- "https://raw.githubusercontent.com/waldronlab/taxPProValidation/main/validation_summary.tsv"
  readr::read_tsv(url, show_col_types = FALSE) |>
    dplyr::filter(.data$rank == "all")
}



# Non exported functions ----------------------------------------------------
.makeSignaturesDiscrete <- function(dat, tax_id_type = "NCBI_ID") {
  dat |>
    dplyr::mutate(
      Attribute = paste0("bugphyzz:", .data$Attribute, "|", .data$Attribute_value)
      ) |>
    {\(y) split(y, y$Attribute)}() |>
    lapply(function(x) unique(x[[tax_id_type]]))
  # if (all(dat$Attribute_group != dat$Attribute)) {
  #   output <- dat |>
  #     dplyr::mutate(
  #       Attribute = paste0("bugphyzz:", .data$Attribute_group, "|", .data$Attribute, "|", .data$Attribute_value)
  #     ) |>
  #     {\(y) split(y, y$Attribute)}() |>
  #     lapply(function(x) unique(x[[tax_id_type]]))
  # } else {
  #   output <- dat |>
  #     dplyr::mutate(
  #       Attribute = paste0("bugphyzz:", .data$Attribute, "|", .data$Attribute_value)
  #     ) |>
  #     {\(y) split(y, y$Attribute)}() |>
  #     lapply(function(x) unique(x[[tax_id_type]]))
  # }
  # return(output)
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
      dplyr::filter(.data$Attribute_group == unique(dat$Attribute))
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

.validationData <- function() {
  url <-  "https://raw.githubusercontent.com/waldronlab/taxPProValidation/main/validation_summary.tsv"
  utils::read.table(
    file = url, header = TRUE, sep = "\t", row.names = NULL
  ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        !is.na(mcc_mean) & is.na(r2_mean) ~ mcc_mean,
        is.na(mcc_mean) & !is.na(r2_mean) ~ r2_mean
      )
    )
}


# Functions what will no longer be used -----------------------------------
## This functiosn will be removed soon
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

whichAttrGrp <- function(bp) {
  sort(unique(bp$Attribute_group))
}

whichAttr <- function(bp) {
  sort(unique(bp$Attribute))
}

