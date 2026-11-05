utils::globalVariables(c(
    "Rank",
    "Attribute", "Attribute_group", "Attribute_new", "Attribute_range",
    "Attribute_value", "Attribute_value_max", "Attribute_value_min", "Br-C10:1",
    "Evidence", "Frequency", "NCBI_ID", "Oxo-C19:1", "Taxon_name", "Unit",
    "attribute", "functionname", "gram_stain", "physiology", "unit", "value",
    ".attr_key", ".value_key"
))

#' Import bugphyzz
#'
#' \code{importBugphyzz} imports bugphyzz annotations as a list of
#' tidy data.frames. To learn more about the structure of the data.frames
#' please check the bugphyzz vignette with `browseVignettes("bugphyzz")` or
#' `vignette("bugphyzz", "bugphyzz").
#'
#' @param version Character string indicating the version. Default is the
#' Zenodo release the package was built against (v1.0.3). Options: a Zenodo
#' DOI, a GitHub commit hash of bugphyzzExports, or "devel" for the weekly
#' export.
#' @param forceDownload Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#' @param mccThreshold Minimum Matthews correlation coefficient (MCC) for
#' keeping ASR annotations of discrete attributes. Default 0.7 (see details).
#' @param r2Threshold Minimum R-squared for keeping ASR annotations of numeric
#' attributes. Default 0.5 (see details).
#' @param excludeRarely Default is TRUE. Exclude annotations with
#' Frequency "rarely" or "never" (see details).
#' @param v Deprecated. A single threshold applied to both MCC and R-squared.
#' If supplied, it overrides `mccThreshold` and `r2Threshold` with a warning.
#'
#' @details
#'
#' ## Data structure
#' The data structure of the data.frames imported with `importBugphyzz` are
#' detailed in the main vignette. Please run `browseVignettes("bugphyzz")`.
#'
#' ## Validation (`mccThreshold` and `r2Threshold` arguments)
#' Data imported with `importBugphyzz` includes annotations imputed through
#' ancestral state reconstruction (ASR) methods. A 10-fold cross-validation
#' approach was implemented to assess the reliability of the data imputed.
#' Matthews correlation coefficient (MCC) and R-squared (R2) were used for the
#' validation of discrete and numeric attributes.
#' Details can be found at: https://github.com/waldronlab/taxPProValidation.
#' By default, imputed annotations are kept when the MCC of the attribute
#' value is at least 0.7 (discrete attributes) or the R2 of the attribute is
#' at least 0.5 (numeric attributes). These are the thresholds used in the
#' bugphyzz manuscript. Set either threshold to 0 to keep all imputed
#' annotations. Curated annotations (any Evidence other than "asr") are never
#' filtered by these thresholds.
#'
#' ## Frequency (excludeRarely argument)
#' One of the variables in the bugphyzz data.frames is "Frequency", which
#' can adopt values of
#' "always", "usually", "sometimes", "rarely", or "never". By default
#' "never" and "rarely" are excluded. They can be included with
#' `excludeRarely = FALSE`. To learn more about these frequency keywords
#' please check the bugphyzz vignette with `browseVignettes("bugphyzz")`.
#' 
#' ## Sources
#' By default, the datasets imported with the `importBugphuzz` function
#' will always return a shortened version of the source. Please use
#' vigette("sources", "bugphyz") to see the full sources.
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
        version = "10.5281/zenodo.12574596", forceDownload = FALSE,
        mccThreshold = 0.7, r2Threshold = 0.5, excludeRarely = TRUE, v = NULL
) {
    if (!is.null(v)) {
        warning(
            "The `v` argument is deprecated. Use `mccThreshold` and",
            " `r2Threshold` instead. Using v = ", v, " for both.",
            call. = FALSE
        )
        mccThreshold <- v
        r2Threshold <- v
    }
    .checkThreshold(mccThreshold, "mccThreshold")
    .checkThreshold(r2Threshold, "r2Threshold")

    ## output is a list of three data.frames
    ## one of each: binary, multistate, numeric
    output <- .downloadResource(version, forceDownload)

    output <- lapply(output, function(x) split(x, x$Attribute))
    output <- purrr::list_flatten(output)

    names(output) <- purrr::map_chr(output, ~ unique(.x$Attribute))
    val <- .validationData() |>
        dplyr::filter(rank == "all") |>
        dplyr::select(physiology, attribute, value) |>
        dplyr::mutate(physiology = tolower(physiology)) |>
        dplyr::mutate(attribute = tolower(attribute))

    output <- .filterAsr(output, val, mccThreshold, r2Threshold)

    if (excludeRarely) {
        output <- purrr::map(
            output,
            ~ dplyr::filter(
                .x, !is.na(Frequency) & !Frequency %in% c("rarely", "never")
            )
        )
    }
    return(output)
}

.checkThreshold <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || x > 1) {
        stop(
            "`", name, "` must be a single number between 0 and 1.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

## Join validation scores and drop ASR annotations below the threshold for
## their attribute type. `output` is a named list of data.frames, one per
## attribute; `val` has columns physiology, attribute (lower case) and value.
.filterAsr <- function(output, val, mccThreshold, r2Threshold) {
    purrr::map(output, ~ {
        attrType <- unique(.x$Attribute_type)
        ## Join on lower-case keys: attribute names in the exports keep the
        ## case of the curation sheets (e.g. "COGEM pathogenicity rating").
        x <- dplyr::mutate(.x, .attr_key = tolower(Attribute))
        ## Binary and numeric attributes are validated as a whole and have
        ## physiology == attribute in the validation table. Multistate values
        ## such as "coccus" repeat across physiologies, so keep them apart.
        valWhole <- dplyr::filter(val, physiology == attribute)
        if (attrType == "binary") {
            val <- dplyr::select(valWhole, .attr_key = attribute, value)
            o <- dplyr::left_join(x, val, by = ".attr_key")
        } else if (
            attrType == "multistate-intersection" ||
            attrType == "multistate-union"
        ) {
            val <- dplyr::select(
                val, .attr_key = physiology, .value_key = attribute, value
            )
            o <- dplyr::left_join(
                dplyr::mutate(
                    x, Attribute_value = tolower(Attribute_value),
                    .value_key = Attribute_value
                ),
                val, by = c(".attr_key", ".value_key")
            ) |>
                dplyr::select(-.value_key)
        } else if (attrType == "numeric") {
            val <- dplyr::select(valWhole, .attr_key = attribute, value)
            o <- dplyr::left_join(x, val, by = ".attr_key") |>
                dplyr::rename(NSTI = nsti)
        }
        thr <- if (attrType == "numeric") r2Threshold else mccThreshold
        o |>
            dplyr::select(-.attr_key) |>
            dplyr::filter(
                Evidence != "asr" | (!is.na(value) & value >= thr)
            ) |>
            dplyr::mutate(value = ifelse(Evidence != "asr", NA, value)) |>
            dplyr::rename(Validation = value)
    })
}

#' Make signatures
#'
#' \code{makeSignatures} Creates signatures for a list of bug signatures from
#' a tidy data.frame imported through the `importBugphyzz` function. Please
#' run `browseVignettes("bugphyz")` for detailed examples.
#'
#' @param dat A data.frame.
#' @param taxIdType A character string. Valid options: NCBI_ID, Taxon_name.
#' @param taxLevel A character vector. Taxonomic rank. Valid options:
#' superkingdom, kingdom, phylum, class, order, family, genus, species, strain.
#' They can be combined. "mixed" is equivalent to select all valid ranks.
#' @param evidence A character vector. Valid options: exp, igc, nas, tas, tax,
#' asr. They can be combined. Default is all.
#' @param frequency A character vector. Valid options: always, usually,
#' sometimes, rarely, unknown. They can be combined. By default, "rarely" is
#' excluded.
#' @param minSize Minimum number of bugs in a signature. Default is 10.
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
        dat,
        taxIdType = c("NCBI_ID", "Taxon_name"),
        taxLevel = c("mixed", "superkingdom", "phylum", "class", "order",
                     "family", "genus", "species", "strain"),
        evidence = c("exp", "igc", "tas", "nas", "tax", "asr"),
        frequency = c("always", "usually", "sometimes", "unknown"),
        minSize = 10, min = NULL, max = NULL
) {
    taxIdType <- match.arg(arg = taxIdType, several.ok = FALSE)
    taxLevel <- match.arg(arg = taxLevel, several.ok = TRUE)
    evidence <- match.arg(arg = evidence, several.ok = TRUE)
    frequency <- match.arg(arg = frequency, several.ok = TRUE)
    
    attrType <- unique(dat$Attribute_type)
    if ("mixed" %in% taxLevel) {
        taxLevel <- c(
            "superkingdom", "phylum", "class", "order", "family", "genus",
            "species", "strain"
        )
    }
    dat <- dat |>
        {\(y) y[which(y$Rank %in% taxLevel),]}() |> 
        {\(y) y[which(y$Evidence %in% evidence),]}() |> 
        {\(y) y[which(y$Frequency %in% frequency),]}()
        
    if (!nrow(dat)) {
        warning(
            "Not enough data for creating signatures.",
            " Try different filtering options",
            call. = FALSE
        )
        return(NULL)
    }
    if (
        attrType %in%
        c("multistate-intersection", "binary", "multistate-union")
        ) {
        s <- .makeSignaturesDiscrete(dat = dat, taxIdType = taxIdType)
    } else if (attrType %in% c("range", "numeric")) {
        s <- .makeSignaturesNumeric(
            dat = dat, taxIdType = taxIdType, min = min, max = max
        )
    }
    output <- purrr::keep(s, ~ length(.x) >= minSize)
    if (!length(output)) {
        warning(
            "Not enough data for creating signatures.",
            " Try different filtering options",
            call. = FALSE
        )
    }
    return(output)
}

#' Get Taxon Signatures
#'
#' \code{getTaxonSignatures} returns the names of all of the signatures
#' associated with a particular taxon. More details can be found in the main
#' bugphyzz vignette; please run `browseVignettes("bugphyzz")`.
#'
#' @param tax A valid NCBI ID or taxon name. If taxon name is used, the
#' argument taxIdType = "Taxon_name" must also be used.
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
#' sig_names_2 <- getTaxonSignatures(taxonName, bp, taxIdType = "Taxon_name")
#'
getTaxonSignatures <- function(tax, bp, ...) {
    sigs <- purrr::map(bp, makeSignatures, ...)
    sigs <- purrr::list_flatten(sigs, name_spec = "{inner}")
    pos <- which(purrr::map_lgl(sigs, ~ tax %in% .x))
    output <- names(sigs)[pos]
    return(output)
}

# Non exported functions ----------------------------------------------------
.makeSignaturesDiscrete <- function(dat, taxIdType = "NCBI_ID") {
    dat$Attribute <- paste0(
        "bugphyzz:", dat$Attribute, "|", dat$Attribute_value
    )
    dat |> 
        {\(y) S4Vectors::split(y, y$Attribute)}() |>
        lapply(function(x) unique(x[[taxIdType]]))
}

.makeSignaturesNumeric <- function(
        dat, taxIdType = "NCBI_ID", min = NULL, max = NULL
) {
    if (!is.null(min) || !is.null(max)) {
        if (is.null(min)) {
            message(
                "Minimum unespecified. Using ", min(dat$Attribute_value), "."
            )
            min <- min(dat$Attribute_value)
        }
        if (is.null(max)) {
            message(
                "Maximum unespecified. Using ", max(dat$Attribute_value), "."
            )
            max <- max(dat$Attribute_value)
        }
        
        dat <- dat[
            which(dat$Attribute_value >= min & dat$Attribute_value <= max),
        ]
        dat$Attribute <- paste0(
            "bugphyzz:", dat$Attribute, "| >=", min, " & <=", max
        )
    } else {
        thr <- .thresholds() |>
            dplyr::filter(Attribute_group == unique(dat$Attribute))
        attrName <- thr$Attribute
        minValues <- thr$lower
        maxValues <- thr$upper
        dat$tmp_col <- NA
        ## This for loop modify a series of vectors rather than creating
        ## a list. Result has been pre-allocated in those vectors.
        for (i in seq_along(attrName)) {
            if (is.na(minValues[i]))
                minValues[i] <- min(dat$Attribute_value) - 0.01
            if (is.na(maxValues[i]))
                maxValues[i] <- max(dat$Attribute_value)
            pos <- which(
                dat$Attribute_value > minValues[i] &
                    dat$Attribute_value <= maxValues[i]
            )
            dat$tmp_col[pos] <- attrName[i]
            dat$Attribute[pos] <- paste0(
                "bugphyzz:", dat$Attribute[pos], "|", attrName[i], "| > ",
                round(minValues[i], 2), " & <= ", maxValues[i]
            )
        }
    }
    dat |>
        {\(y) S4Vectors::split(y, y$Attribute)}() |>
        lapply(function(x) unique(x[[taxIdType]]))
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
    fname <- system.file(
        "extdata", "validation_summary.tsv", package = "bugphyzz"
    )
    utils::read.table(
        file = fname, header = TRUE, sep = "\t", row.names = NULL
    ) |>
        dplyr::mutate(
            value = dplyr::case_when(
                !is.na(mcc_mean) & is.na(r2_mean) ~ mcc_mean,
                is.na(mcc_mean) & !is.na(r2_mean) ~ r2_mean
            )
        )
}

## Import a version of bupghyzz
.downloadResource <- function(version, forceDownload) {
    if (stringr::str_detect(version, "^10.5281/zenodo.[0-9]+$")) {
        suffix <- sub("^10.5281/zenodo\\.", "", version)
        output <- .downloadZ(suffix, forceDownload)
    } else if (
        version == "devel" ||
            stringr::str_detect(version, stringr::regex("^[:alnum:]{7}$"))
        ){
        output <- .downloadGH(version, forceDownload)
    } else {
        stop("Version must be a Zenodo DOI, GitHub commit hash, or 'devel'.")
    }
    return(output)
}

## Function for downloading data from Zenodo
.downloadZ <- function(record, forceDownload) {
    baseUrl <- paste0("https://zenodo.org/api/records/", record)
    req <- httr2::request(baseUrl)
    res <- httr2::req_perform(req)
    l <- httr2::resp_body_json(res)
    fileNamesApi <- purrr::map_chr(l$files, ~ .x$links$self)
    fileNamesUrl <- sub(
        "(^.*)(api/)(.*)(/content$)", "\\1\\3", fileNamesApi
    )
    rpath <- .getResource(
        rname = paste0("bugphyzz.zip"),
        url = fileNamesUrl, verbose = TRUE, force = forceDownload
    )
    tempDir <- tempdir()
    utils::unzip(zipfile = rpath, exdir = tempDir, junkpaths = TRUE)
    ## Only the three export tables; the archive also holds sources/*.csv.gz
    files <- list.files(
        tempDir, pattern = "^bugphyzz_(binary|multistate|numeric)\\.csv$",
        full.names = TRUE
    )
    lapply(files, function(x) {
        utils::read.csv(x, header = TRUE, skip = 1) |> 
            dplyr::mutate(Attribute = tolower(Attribute))
    })
}

## Function for downloading data from GitHub
.downloadGH <- function(version, forceDownload) {
    fileSuffix <- c("binary", "multistate", "numeric")
    urls <- paste0(
        "https://github.com/waldronlab/bugphyzzExports/raw/", version,
        "/bugphyzz_", fileSuffix, ".csv"
    )
    names(urls) <-  c("binary", "multistate", "numeric")
    lapply(seq_len(length(urls)), function(i) {
        message("Importing ", names(urls)[i], " data...")
        rpath <- .getResource(
            rname = paste0("bugphyzz_", names(urls)[i], ".csv"),
            url = urls[i], verbose = TRUE, force = forceDownload
        )
        utils::read.csv(rpath, header = TRUE, skip = 1) |>
            dplyr::mutate(Attribute = tolower(Attribute))
    })
}
