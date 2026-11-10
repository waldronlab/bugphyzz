utils::globalVariables(c(
    "Rank",
    "Attribute", "Attribute_group", "Attribute_new", "Attribute_range",
    "Attribute_value", "Attribute_value_max", "Attribute_value_min", "Br-C10:1",
    "Evidence", "Frequency", "NCBI_ID", "Oxo-C19:1", "Taxon_name", "Unit",
    "attribute", "functionname", "gram_stain", "physiology", "unit", "value",
    ".attr_key", ".value_key", "score_lcb", "rank", "Attribute_type"
))

#' Import bugphyzz
#'
#' \code{importBugphyzz} imports bugphyzz annotations as a list of
#' tidy data.frames. To learn more about the structure of the data.frames
#' please check the bugphyzz vignette with `browseVignettes("bugphyzz")` or
#' `vignette("bugphyzz", "bugphyzz").
#'
#' @param version Character string indicating the version. Default is the
#' Zenodo release the package was built against (v1.1.0, November 2026). Options: a Zenodo
#' DOI, a GitHub commit hash of bugphyzzExports, or "devel" for the weekly
#' export.
#' @param forceDownload Logical value. Force a fresh download of the data or
#' use the one stored in the cache (if available). Default is FALSE.
#' @param kappaThreshold Minimum cross-validated Cohen's kappa (lower 95%
#' bound) for keeping the imputed annotations of a discrete attribute.
#' Default 0.7 (see details).
#' @param r2Threshold Minimum cross-validated R-squared (lower 95% bound) for
#' keeping the imputed annotations of a numeric attribute. Default 0.5 (see
#' details).
#' @param asrScoreThreshold Minimum `Score` of an imputed annotation.
#' Default 0.5 (see details).
#' @param excludeRarely Deprecated. `excludeRarely = FALSE` is equivalent to
#' `asrScoreThreshold = 0`; curated annotations with Frequency "rarely" or
#' "never" are always excluded.
#' @param v Deprecated. A single threshold applied to both `kappaThreshold`
#' and `r2Threshold`.
#'
#' @details
#'
#' ## Data structure
#' The data structure of the data.frames imported with `importBugphyzz` are
#' detailed in the main vignette. Please run `browseVignettes("bugphyzz")`.
#'
#' ## Imputed annotations (`kappaThreshold`, `r2Threshold`)
#' Data imported with `importBugphyzz` includes annotations imputed through
#' ancestral state reconstruction (ASR), with Evidence "asr". An attribute's
#' imputed annotations are kept all together or not at all, based on a
#' cross-validation of the reconstruction
#' (https://github.com/waldronlab/taxPProValidation): Cohen's kappa for
#' discrete attributes (linearly weighted for attributes with ordered
#' states, such as aerophilicity) and R-squared for numeric attributes, with
#' held-out taxa weighted to match the phylogenetic distance of the taxa to
#' be imputed. The threshold applies to the lower bound of the 95%
#' confidence interval of the score. The defaults are the thresholds applied
#' by the export (bugphyzzExports ADR 0006). Curated annotations are never
#' filtered by these thresholds.
#'
#' ## Score of imputed annotations (`asrScoreThreshold`)
#' For discrete attributes, `Score` is the posterior probability of the
#' imputed state. For numeric attributes it is a step function of the
#' distance to the nearest annotated taxon (NSTI): 0.9 up to 0.05, 0.5 up to
#' 0.15, 0.1 up to 0.2 and 0 beyond. Raise `asrScoreThreshold` to keep fewer,
#' more reliable imputed annotations.
#'
#' ## Thresholds below the defaults
#' The export removes imputed annotations below the default thresholds and
#' keeps them in a separate file. Setting any threshold below its default
#' makes `importBugphyzz` read that file as well, for versions that have it.
#'
#' ## Frequency
#' Curated annotations with Frequency "rarely" or "never" are excluded. Use
#' `physiologies()` for all curated data.
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
        version = "10.5281/zenodo.24114812", forceDownload = FALSE,
        kappaThreshold = 0.7, r2Threshold = 0.5, asrScoreThreshold = 0.5,
        excludeRarely, v = NULL
) {
    if (!is.null(v)) {
        warning(
            "The `v` argument is deprecated. Use `kappaThreshold` and",
            " `r2Threshold` instead. Using v = ", v, " for both.",
            call. = FALSE
        )
        kappaThreshold <- v
        r2Threshold <- v
    }
    if (!missing(excludeRarely)) {
        warning(
            "The `excludeRarely` argument is deprecated. Use",
            " `asrScoreThreshold` instead.", call. = FALSE
        )
        if (isFALSE(excludeRarely)) asrScoreThreshold <- 0
    }
    .checkThreshold(kappaThreshold, "kappaThreshold")
    .checkThreshold(r2Threshold, "r2Threshold")
    .checkThreshold(asrScoreThreshold, "asrScoreThreshold")

    ## output is a list of three data.frames
    ## one of each: binary, multistate, numeric
    belowDefaults <- kappaThreshold < .exportThresholds[["kappa"]] ||
        r2Threshold < .exportThresholds[["r2"]] ||
        asrScoreThreshold < .exportThresholds[["score"]]
    output <- .downloadResource(version, forceDownload, dropped = belowDefaults)

    output <- lapply(output, function(x) split(x, x$Attribute))
    output <- purrr::list_flatten(output)
    names(output) <- purrr::map_chr(output, ~ unique(.x$Attribute))

    val <- .validationAttributes() |>
        dplyr::filter(rank == "all") |>
        dplyr::transmute(
            .attr_key = tolower(physiology), Validation = score_lcb
        )
    .filterAsr(output, val, kappaThreshold, r2Threshold, asrScoreThreshold)
}

## Thresholds the export applies (bugphyzzExports ADR 0006). Imputed rows
## below them are in bugphyzz_asr_dropped.csv.gz, not in the main tables.
.exportThresholds <- c(kappa = 0.7, r2 = 0.5, score = 0.5)

## Append the dropped imputed rows to the three main tables. `dropped` is
## the side file read as character; each column gets the type it has in the
## main table (numeric for integer columns) so bind_rows() can't fail on a
## type clash or truncate a fractional Score.
.bindDropped <- function(output, dropped) {
    dropped <- dropped |>
        dplyr::mutate(Attribute = tolower(Attribute)) |>
        dplyr::select(-dplyr::any_of("Validation"))
    types <- c("binary", "multistate", "numeric")
    lapply(seq_along(output), function(i) {
        part <- if (types[i] == "multistate") {
            dplyr::filter(dropped, grepl("^multistate", Attribute_type))
        } else {
            dplyr::filter(dropped, Attribute_type == types[i])
        }
        part <- part[intersect(names(output[[i]]), names(part))]
        for (col in names(part)) {
            target <- output[[i]][[col]]
            part[[col]] <- if (is.numeric(target)) {
                ## integer when every value happened to be whole (Score 1)
                as.numeric(part[[col]])
            } else {
                methods::as(part[[col]], class(target)[1])
            }
        }
        dplyr::bind_rows(output[[i]], part)
    })
}

.noDroppedWarning <- function(version) {
    warning(
        "Version ", version, " has no file of dropped imputed annotations;",
        " thresholds below the defaults keep only what it ships.",
        call. = FALSE
    )
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

## Keep curated rows except Frequency "rarely"/"never" (or missing), and
## imputed rows whose attribute passes its threshold and whose Score is at
## least asrScoreThreshold. `output` is a named list of data.frames, one per
## attribute; `val` has one row per attribute: .attr_key (lower case) and
## Validation (lower 95% bound of kappa or R2).
.filterAsr <- function(output, val, kappaThreshold, r2Threshold,
                       asrScoreThreshold) {
    purrr::map(output, ~ {
        attrType <- unique(.x$Attribute_type)
        thr <- if (attrType == "numeric") r2Threshold else kappaThreshold
        o <- dplyr::mutate(.x, .attr_key = tolower(Attribute)) |>
            dplyr::left_join(val, by = ".attr_key") |>
            dplyr::select(-.attr_key)
        if ("nsti" %in% names(o)) o <- dplyr::rename(o, NSTI = nsti)
        isAsr <- o$Evidence %in% "asr"
        keepAsr <- !is.na(o$Validation) & o$Validation >= thr &
            !is.na(o$Score) & o$Score >= asrScoreThreshold
        keepCurated <- !is.na(o$Frequency) &
            !o$Frequency %in% c("rarely", "never")
        o <- o[ifelse(isAsr, keepAsr, keepCurated), , drop = FALSE]
        o$Validation[!o$Evidence %in% "asr"] <- NA
        ## multistate values have always been returned in lower case
        if (grepl("^multistate", attrType)) {
            o$Attribute_value <- tolower(o$Attribute_value)
        }
        o
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

## Attribute-level cross-validation scores (bugphyzzExports ADR 0006), from
## waldronlab/taxPProValidation: one row per rank and physiology, with the
## lower 95% bound of kappa (discrete) or R2 (numeric) in score_lcb.
.validationAttributes <- function() {
    fname <- system.file(
        "extdata", "validation_attributes.tsv", package = "bugphyzz"
    )
    utils::read.table(
        file = fname, header = TRUE, sep = "\t", row.names = NULL,
        quote = "", comment.char = ""
    )
}

## Import a version of bugphyzz. With dropped = TRUE, the imputed rows the
## export removed (bugphyzz_asr_dropped.csv.gz) are appended where the
## version has them.
.downloadResource <- function(version, forceDownload, dropped = FALSE) {
    if (stringr::str_detect(version, "^10.5281/zenodo.[0-9]+$")) {
        suffix <- sub("^10.5281/zenodo\\.", "", version)
        output <- .downloadZ(suffix, forceDownload, dropped)
    } else if (
        version == "devel" ||
            stringr::str_detect(version, stringr::regex("^[:alnum:]{7}$"))
        ){
        output <- .downloadGH(version, forceDownload, dropped)
    } else {
        stop("Version must be a Zenodo DOI, GitHub commit hash, or 'devel'.")
    }
    return(output)
}

.downloadZ <- function(record, forceDownload, dropped = FALSE) {
    baseUrl <- paste0("https://zenodo.org/api/records/", record)
    req <- httr2::request(baseUrl)
    res <- httr2::req_perform(req)
    l <- httr2::resp_body_json(res)
    fileNamesApi <- purrr::map_chr(l$files, ~ .x$links$self)
    fileNamesUrl <- sub(
        "(^.*)(api/)(.*)(/content$)", "\\1\\3", fileNamesApi
    )
    rpath <- .getResource(
        rname = paste0("bugphyzz_zenodo_", record, ".zip"),
        url = fileNamesUrl, verbose = TRUE, force = forceDownload
    )
    out <- .readZenodoZip(rpath, dropped)
    if (!dropped) return(out$tables)
    if (is.null(out$dropped)) {
        .noDroppedWarning(paste0("10.5281/zenodo.", record))
        return(out$tables)
    }
    .bindDropped(out$tables, out$dropped)
}

## A Zenodo deposit is the GitHub archive of a bugphyzzExports tag, so it also
## carries sources/*.csv.gz and other files. Only the three export tables
## (and, if asked, the dropped imputations) are read.
.readZenodoZip <- function(rpath, dropped = FALSE) {
    exdir <- tempfile("bugphyzz")
    dir.create(exdir)
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    utils::unzip(zipfile = rpath, exdir = exdir, junkpaths = TRUE)
    files <- list.files(
        exdir, pattern = "^bugphyzz_(binary|multistate|numeric)\\.csv$",
        full.names = TRUE
    )
    tables <- lapply(files, function(x) {
        utils::read.csv(x, header = TRUE, skip = 1) |>
            dplyr::mutate(Attribute = tolower(Attribute))
    })
    if (!dropped) return(list(tables = tables, dropped = NULL))
    droppedFile <- file.path(exdir, "bugphyzz_asr_dropped.csv.gz")
    droppedTable <- if (file.exists(droppedFile)) .readDropped(droppedFile)
    list(tables = tables, dropped = droppedTable)
}

## FALSE only for a 404 (older versions have no dropped file); other
## failures are errors.
.urlExists <- function(url) {
    resp <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    status <- httr2::resp_status(resp)
    if (status == 404) return(FALSE)
    if (status >= 400) {
        stop("Could not reach ", url, " (HTTP ", status, ").", call. = FALSE)
    }
    TRUE
}

.readDropped <- function(path) {
    utils::read.csv(
        gzfile(path), header = TRUE, skip = 1, colClasses = "character"
    )
}

.downloadGH <- function(version, forceDownload, dropped = FALSE) {
    fileSuffix <- c("binary", "multistate", "numeric")
    urls <- paste0(
        "https://github.com/waldronlab/bugphyzzExports/raw/", version,
        "/bugphyzz_", fileSuffix, ".csv"
    )
    names(urls) <-  c("binary", "multistate", "numeric")
    tables <- lapply(seq_len(length(urls)), function(i) {
        message("Importing ", names(urls)[i], " data...")
        rpath <- .getResource(
            rname = paste0("bugphyzz_", names(urls)[i], "_", version, ".csv"),
            url = urls[i], verbose = TRUE, force = forceDownload
        )
        utils::read.csv(rpath, header = TRUE, skip = 1) |>
            dplyr::mutate(Attribute = tolower(Attribute))
    })
    if (!dropped) return(tables)
    message("Importing dropped imputed annotations...")
    url <- paste0(
        "https://github.com/waldronlab/bugphyzzExports/raw/", version,
        "/bugphyzz_asr_dropped.csv.gz"
    )
    if (!.urlExists(url)) {
        .noDroppedWarning(version)
        return(tables)
    }
    rpath <- .getResource(
        rname = paste0("bugphyzz_asr_dropped_", version, ".csv.gz"),
        url = url, verbose = TRUE, force = forceDownload
    )
    .bindDropped(tables, .readDropped(rpath))
}
