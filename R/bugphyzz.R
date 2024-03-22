utils::globalVariables(c(
  "Rank",
  "Attribute", "Attribute_group", "Attribute_new", "Attribute_range",
  "Attribute_value", "Attribute_value_max", "Attribute_value_min", "Br-C10:1",
  "Evidence", "Frequency", "NCBI_ID", "Oxo-C19:1", "Taxon_name", "Unit",
  "attribute", "functionname", "gram_stain", "physiology", "unit", "value"
))

#' Import bugphyzz
#'
#' \code{importBugphyzz} imports bugphyzz annotations as a list of
#' tidy data.frames. To learn more about the structure of the data.frames
#' please check the bugphyzz vignette with `browseVignettes("bugphyzz")`.
#'
#' @param version Character string indicating the version.
#' Options: devel, doi, GitHub hash.
#' @param force_download Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#' @param v Validation value. Default 0.5 (see details).
#' @param exclude_rarely Default is TRUE. Exclude values with
#' Frequency == FALSE (see details).
#'
#' @details
#'
#' ## Data structure
#' The data structure of the data.frames imported with `importBugphyzz` are
#' detailed in the main vignette. Please run `browseVignettes("bugphyzz")`.
#'
#' ## Validation (`v` argument)
#' Data imported with `importBugphyzz` includes annotations imputed through
#' ancestral state reconstruction (ASR) methods. A 10-fold cross-validation
#' approach was implemented to assess the reliability of the data imputed.
#' Mathew's correlation coefficient (MCC) and R-squared (R2) were used for the
#' validation of discrete and numeric attributes.
#' Details can be found at: https://github.com/waldronlab/taxPProValidation.
#' By default, imputed annotations with a MCC or R2 value greater than 0.5 are
#' imported. The minimum value can be adjusted with the `v` argument (only
#' values between 0 and 1).
#'
#' ## Frequency (exclude_rarely argument)
#' One of the variables in the bugphyzz data.frames is "Frequency", which
#' can adopt values of
#' "always", "usually", "sometimes", "rarely", or "never". By default
#' "never" and "rarely" are excluded. "rarely" could be included with
#' `exclude_rarely = FALSE`. To learn more about these frequency keywords
#' please check the bugphyzz vignette with `browseVignettes("bugphyzz")`.
#'
#' @return A list of tidy data frames.
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
  if (version == 'devel') {
    output <- .downloadDevel(force_download)
  }
  ## TODO add release version
  output <- lapply(output, function(x) split(x, x$Attribute))
  output <- purrr::list_flatten(output)

  ## TODO correct plant pathogenicity name earlier in the workflow or
  ## better yet, directly in the curation
  pos <- which(names(output) == "plant pathogenity")
  names(output)[pos] <- "plant pathogenicity"
  output <- purrr::map(output, ~ {
    .x |>
      dplyr::mutate(
        Attribute = ifelse(
          Attribute == "plant pathogenity",
          "plant pathogenicity",
          Attribute
        )
      )
  })

  names(output) <- purrr::map_chr(output, ~ unique(.x$Attribute))
  val <- .validationData() |>
    dplyr::filter(rank == "all") |>
    dplyr::select(physiology, attribute, value) |>
    dplyr::mutate(physiology = tolower(physiology)) |>
    dplyr::mutate(attribute = tolower(attribute))

  output <- purrr::map(output, ~ {
    attr_type <- unique(.x$Attribute_type)
    if (attr_type == "binary") {
      val <- dplyr::select(val, Attribute = attribute, value)
      o <- dplyr::left_join(.x, val, by = "Attribute" )
    } else if (attr_type == "multistate-intersection" || attr_type == "multistate-union") {
      val <- dplyr::select(val, Attribute = physiology, Attribute_value = attribute, value)
      o <- dplyr::left_join(dplyr::mutate(.x, Attribute_value = tolower(Attribute_value)) , val, by = c("Attribute", "Attribute_value"))
    } else if (attr_type == "numeric") {
      val <- dplyr::select(val, Attribute = attribute, value)
      o <- dplyr::left_join(.x, val, by = "Attribute") |>
        dplyr::rename(NSTI = nsti)
    }
    o |>
      dplyr::filter(
       !(value < v & Evidence == "asr")
      ) |>
      dplyr::mutate(value = ifelse(Evidence != "asr", NA, value)) |>
      dplyr::rename(Validation = value)
  })

  if (exclude_rarely) {
    output <- purrr::map(output, ~ dplyr::filter(.x, Frequency != "rarely"))
  }
  return(output)
}

#' Make signatures
#'
#' \code{makeSignatures} Creates signatures for a list of bug signatures from
#' a tidy data.frame imported through the `importBugphyzz` function. Please
#' run `browseVignettes("bugphyz")` for detailed examples.
#'
#' @param dat A data.frame.
#' @param tax_id_type A character string. Valid options: NCBI_ID, Taxon_name.
#' @param tax_level A character vector. Taxonomic rank. Valid options:
#' superkingdom, kingdom, phylum, class, order, family, genus, species, strain.
#' They can be combined. "mixed" is equivalent to select all valid ranks.
#' @param evidence A character vector. Valid options: exp, igc, nas, tas, tax, asr.
#' They can be combined. Default is all.
#' @param frequency A character vector. Valid options: always, usually,
#' sometimes, rarely, unknown. They can be combined. By default, "rarely" is
#' excluded.
#' @param min_size Minimum number of bugs in a signature. Default is 10.
#' @param min Minimum value (inclusive). Only for numeric attributes.
#' Default is NULL.
#' @param max Maximum value (inclusive). Only for numeric attributes.
#' Default is NULL.
#'
#' @return A list of character vectors with scientific names or taxids.
#' @export
#'
#' @examples
#'
#' bp <- importBugphyzz()
#' sigs <- purrr::map(bp, makeSignatures)
#' sigs <- purrr::list_flatten(sigs, name_spec = "{inner}")
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
    dplyr::filter(Evidence %in% evidence) |>
    dplyr::filter(Frequency %in% frequency)
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
#' \code{getTaxonSignatures} returns the names of all of the signatures associated
#' with a particular taxon. More details can be found in the main
#' bugphyzz vignette; please run `browseVignettes("bugphyzz")`.
#'
#' @param tax A valid NCBI ID or taxon name. If taxon name is used, the
#' argument tax_id_type = "Taxon_name" must also be used.
#' @param bp List of data.frames imported with \code{importBugphyzz}.
#' @param ... Arguments passed to \code{makeSignatures}.
#'
#' @return A character vector with the names of the signatures for a taxon.
#' @export
#'
#' @examples
#' taxid <- "562"
#' taxonName <- "Escherichia coli"
#' bp <- importBugphyzz()
#' sig_names_1 <- getTaxonSignatures(taxid, bp)
#' sig_names_2 <- getTaxonSignatures(taxonName, bp, tax_id_type = "Taxon_name")
#'
getTaxonSignatures <- function(tax, bp, ...) {
  sigs <- purrr::map(bp, makeSignatures, ...)
  sigs <- purrr::list_flatten(sigs, name_spec = "{inner}")
  pos <- which(purrr::map_lgl(sigs, ~ tax %in% .x))
  output <- names(sigs)[pos]
  return(output)
}

# Non exported functions ----------------------------------------------------
.makeSignaturesDiscrete <- function(dat, tax_id_type = "NCBI_ID") {
  dat |>
    dplyr::mutate(
      Attribute = paste0("bugphyzz:", Attribute, "|", Attribute_value)
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
        Attribute_value >= min & Attribute_value <= max
      ) |>
      dplyr::mutate(
        Attribute = paste0("bugphyzz:", Attribute, "| >=", min, " & <=", max)
      )
  } else {
    thr <- .thresholds() |>
      dplyr::filter(Attribute_group == unique(dat$Attribute))
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
        is.na(lower) ~ paste0('<=', upper),
        is.na(upper) ~ paste0('>=', lower),
        TRUE ~ paste0(lower, '-', upper)
      ),
      unit = ifelse(is.na(unit), '', unit)
    ) |>
    dplyr::mutate(Attribute_range = paste0(range, unit)) |>
    dplyr::relocate(
      Attribute_group, Attribute, Attribute_range
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

## Import the devel version of bupghyzz
.downloadDevel <- function(force_download) {
  types <- c("multistate", "binary", "numeric")
  urls <- paste0(
    "https://github.com/waldronlab/bugphyzzExports/raw/main/bugphyzz_",
    types,
    ".csv"
  )
  names(urls) <- types
  output <- vector("list", length(urls))
  for (i in seq_along(output)) {
    message("Importing ", names(urls)[i], " data...")
    names(output)[i] <- names(urls)[i]
    rpath <- .getResource(
      rname = paste0("bugphyzz_", names(urls)[i], ".tsv"),
      url = urls[i], verbose = TRUE, force = force_download
    )
    output[[i]] <- utils::read.csv(rpath, header = TRUE, skip = 1) |>
      dplyr::mutate(Attribute = tolower(Attribute))
  }
  return(output)
}

## TODO update this function when relase is ready
.downloadZ <- function(record, force_download) {
  base_url <- paste0("https://zenodo.org/api/records/", record)
  req <- httr2::request(base_url)
  res <- httr2::req_perform(req)
  l <- httr2::resp_body_json(res)

  file_names_api <- purrr::map_chr(l$files, ~ .x$links$self)
  file_names_url <- sub("(^.*)(api/)(.*)(/content$)", "\\1\\3", file_names_api)

  rpath <- .getResource(
    rname = paste0("bugphyzz.zip"),
    url = file_names_url, verbose = TRUE, force = force_download
  )
  temp_dir <- tempdir()
  utils::unzip(zipfile = rpath, exdir = temp_dir, junkpaths = TRUE)
  files <- list.files(temp_dir, pattern = "csv", full.names = TRUE)

  output <- vector("list", length(files))
  for (i in seq_along(output)) {
    output[[i]] <- utils::read.csv(files[i], header = TRUE)
    # output[[i]] <- utils::read.csv(files, header = TRUE, skip = 1)
    # dplyr::mutate(Attribute = tolower(Attribute))
  }
  return(output)
}
