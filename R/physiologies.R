
#' Import physiologies (for devs)
#'
#' \code{physiologies} imports a list of data.frames. This data is in "raw"
#' state before cleaning and going through the data imputation steps. It
#' should be used by developers/curators of the package.
#'
#' @param keyword Character vector with one or more valid keywords.
#' Valid keyboards can be checked with \code{showPhys}. If 'all', all
#' physiologies are imported.
#' @param fullSource Logical. If `TRUE`, the Attribute_source column will
#' contain full source information. If `FALSE`, the Attribute_source column
#' will contain shortened versions of the sources. Default is `FALSE`.
#' @param sources Logical. Also read the programmatic sources (Bergey's
#' Manual extraction, Madin et al. 2020, ...) kept under `sources/` in
#' \url{https://github.com/waldronlab/bugphyzzExports}. Default is `TRUE`.
#' @param forceDownload Logical. Re-download the programmatic sources instead
#' of using the cached copy. Default is `FALSE`. The location can be changed
#' with `options(bugphyzz.sourcesBaseUrl = ...)`, e.g. to a local checkout.
#'
#' @details
#' Rows come from three places: the curation Google Sheets listed in
#' `inst/extdata/spreadsheet_links.tsv`, the BacDive strain table (built from
#' the BacDive API, `sources/bacdive` in bugphyzzExports), and the other
#' programmatic sources above. The last are produced by scripts, never edited
#' by hand, and are cached with BiocFileCache like the exports.
#'
#' Attribute values from all three are mapped to the controlled vocabulary
#' in `inst/extdata/attributes.tsv` with the synonym table
#' `inst/extdata/attribute_synonyms.tsv` (columns `attribute_group`,
#' `source_term`, `attribute`), e.g. BacDive's "rod-shaped" to "bacillus".
#' A source term can map to several attributes; an empty `attribute` drops
#' the row on purpose.
#'
#' @return A list of data.frames in tidy format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Reads every curation sheet and source over the network (several minutes)
#' l <- physiologies('all')
#' df <- physiologies('aerophilicity')[[1]]
#' }
#'
physiologies <- function(
        keyword = 'all', fullSource = FALSE, sources = TRUE,
        forceDownload = FALSE
) {
    keyword <- .checkKeyword(keyword)
    cond1 <- any(keyword %in% showPhys('spreadsheets'))
    cond2 <- any(keyword %in% showPhys('bacdive'))

    if (cond1 && cond2) {
        spreadsheets <- .importSpreadsheets(keyword = keyword)
        spreadsheets <- spreadsheets[names(spreadsheets) %in% keyword]
        bacdive <- .reshapeBacDive(
            .getBacDive(verbose = FALSE, forceDownload = forceDownload)
        )
        bacdive <- bacdive[names(bacdive) %in% keyword]
        physiologies <- lapply(seq_along(keyword), function(i) {
            df1 <- spreadsheets[[keyword[i]]]
            df2 <- bacdive[[keyword[i]]]
            o <- dplyr::bind_rows(df1, df2)
            message('Finished ', keyword[i], '.')
            o
        })
        names(physiologies) <- keyword
    } else if (cond1 && !cond2) {
        spreadsheets <- .importSpreadsheets(keyword = keyword)
        physiologies <- spreadsheets[names(spreadsheets) %in% keyword]
        ## Not creating a vector. Only usin the side effect of the for loop.
        for (i in seq_along(keyword)) {
            message('Finished ', keyword[i], '.')
        }
    } else if (!cond1 && cond2) {
        bacdive <- .reshapeBacDive(
            .getBacDive(verbose = FALSE, forceDownload = forceDownload)
        )
        physiologies <- bacdive[names(bacdive) %in% keyword]
        ## Not creating a vector. Only using the side effect of the for loop.
        for (i in seq_along(keyword)) {
            message('Finished ', keyword[i], '.')
        }
    }

    ## Programmatic sources kept in waldronlab/bugphyzzExports, sources/
    if (sources) {
        src <- tryCatch(
            .importSources(keyword = keyword, forceDownload = forceDownload),
            error = function(e) {
                warning(
                    "Programmatic sources could not be read (",
                    conditionMessage(e), "). Using sheets only.", call. = FALSE
                )
                list()
            }
        )
        for (k in names(src)) {
            physiologies[[k]] <- dplyr::bind_rows(physiologies[[k]], src[[k]])
        }
    }
    physiologies <- physiologies[!vapply(physiologies, is.null, logical(1))]

    physiologies <- lapply(physiologies, function(df) {
        df <- df |>
            purrr::modify_if(.p = is.character, ~ stringr::str_squish(.x)) |>
            .addSourceInfo() |>
            purrr::modify_at(
                .at = c('Frequency', 'Evidence', 'Confidence_in_curation'),
                ~ stringr::str_squish(stringr::str_to_lower(.x))
            ) |>
            dplyr::distinct() |>
            .applySynonyms()
        ## the synonym table can drop every row of a group
        if (!nrow(df)) return(df)

        if (fullSource) {
            df$Attribute_source <- df$fullSource
        }
        df$full_source <- NULL

        df <- .reorderColumns(
            df = df,
            name = unique(df$Attribute_group),
            attrType = unique(df$Attribute_type)
        )

        df <- as.data.frame(df[, vapply(df, \(y) !all(is.na(y)), logical(1))])

        if (unique(df$Attribute_group) == 'aerophilicity') {
            df <- .homogenizeAerophilicityAttributeNames(df)
        }

        dplyr::distinct(df)
    })

    return(physiologies)
}

#' Show list of available physiologies (for devs)
#'
#' \code{showPhys} prints the names of the available physiologies that can be
#' imported with the \code{\link{physiologies}} function. This function
#' should be used by developers/curators.
#'
#' @param whichNames A character string. Options: 'all' (default),
#' 'spreadsheets', 'bacdive'.
#'
#' @return A character vector with the names of the physiologies.
#' @export
#'
#' @examples
#' showPhys()
#' showPhys('bacdive')
#' showPhys('spreadsheets')
#'
showPhys <- function(whichNames = 'all') {
    fname <- system.file(
        'extdata', 'spreadsheet_links.tsv', package = 'bugphyzz'
    )
    links <- utils::read.table(fname, header = TRUE, sep = '\t')
    spreadsheetPhys <- links[['physiology']]
    if (whichNames == 'all')
        ## bacdivePhysNames is a character vector saved as internal data
        physNames <- sort(unique(c(spreadsheetPhys, bacdivePhysNames)))
    if (whichNames == 'spreadsheets')
        physNames <- spreadsheetPhys
    if (whichNames == 'bacdive')
        physNames <- bacdivePhysNames
    return(physNames)
}

## Helper function for physiologies
.checkKeyword <- function(keyword) {
    keyword <- unique(sort(keyword))
    if ('all' %in% keyword) {
        if (length(keyword) > 1) {
            stop(
                "Found 'all' among the keywords.",
                "Are you sure that you want to import all of the physiologies?",
                "If so, use 'all' alone. Quitting.",
                call. = FALSE
            )
        } else if (length(keyword) == 1) {
            message('All physiologies will be imported.')
            keyword <- showPhys()
        }
    }
    validKeywords <- showPhys()
    lglVct <- keyword %in% validKeywords
    if (any(!lglVct) ) {
        invalidKeywords <- keyword[!lglVct]
        stop(
            "Invalid keyword(s): ",
            paste0(invalidKeywords, collapse = ', '), '.',
            " Check valid keywords with showPhys() or use 'all' to import all",
            " physiologies.",
            call. = FALSE
        )
    }
    return(keyword)
}

## Helper function for physiologies
.importSpreadsheets <- function(keyword) {
    fname <- system.file(
        'extdata', 'spreadsheet_links.tsv', package = 'bugphyzz'
    )
    links <- utils::read.table(fname, header = TRUE, sep = '\t')
    links <- links[links[['physiology']] %in% keyword,]
    ## a physiology with no sheet link comes only from programmatic sources
    links <- links[!is.na(links[['link']]) & nzchar(links[['link']]),]
    spreadsheets <- lapply(seq_len(nrow(links)), function(i) {
        physName <- links[i, 'physiology', drop = FALSE][[1]]
        attrType <- links[i, 'attribute_type', drop = FALSE][[1]]
        url <- links[i, 'link', drop = FALSE][[1]]
        df <- dplyr::distinct(utils::read.csv(url))
        .processSheet(df, physName = physName, attrType = attrType)
    })
    names(spreadsheets) <- links$physiology
    return(spreadsheets)
}

## Shared by .importSpreadsheets and .importSources
.processSheet <- function(df, physName, attrType) {
    parentColNames <- c('Parent_name', 'Parent_NCBI_ID', 'Parent_rank')
    df[['Attribute_type']] <- attrType
    df[['Attribute_group']] <- physName
    df[['NCBI_ID']] <- as.character(df[['NCBI_ID']])
    df <- df[!is.na(df[['Attribute_value']]),]

    if (unique(df[['Attribute_type']]) == 'numeric') {
        df <- .numericToRange(df)
    } else if (unique(df[['Attribute_type']] == 'range')) {
        df <- .modifyRange(df)
    } else if (
        unique(df[['Attribute_type']] %in% .discreteAttributeTypes())
    ) {
        df <- dplyr::filter(
            df, Attribute_value == TRUE | Attribute_value == FALSE
        )
    }
    if (all(parentColNames %in% colnames(df))) {
        df$Parent_NCBI_ID <- stringr::str_squish(
            as.character(df$Parent_NCBI_ID)
        )
    } else {
        ## ranksParents is an internal object (data.frame) in bugphyzz
        rp <- purrr::modify_at(
            .x = ranksParents,
            .at = c('NCBI_ID', 'Parent_NCBI_ID'),
            .f = as.character
        )
        df <- dplyr::left_join(df, rp, by = "NCBI_ID")
    }
    df
}

## Helper function for .importSpreadsheets
.numericToRange <- function(df) {
    df <- df |>
        dplyr::group_by(NCBI_ID, Taxon_name) |>
        dplyr::mutate(
            Attribute_value_min = as.double(Attribute_value),
            Attribute_value_max = as.double(Attribute_value),
            Attribute_type = 'range'
        ) |>
        dplyr::ungroup() |>
        dplyr::distinct()
    df[['Attribute_value']] <- NULL
    return(df)
}

## Helper function for .importSpreadsheets
.modifyRange <- function(df) {
    num <- '[0-9]+(\\.[0-9]+)?'
    regex1 <- paste0('^\\-?', num, '(\\-', num, ')?$')
    regex2 <- paste0('^(<|>)(\\-)?', num, '$')
    regex <- paste0('(', regex1, '|', regex2, ')')
    df <- df |>
        dplyr::filter(grepl(regex, Attribute_value)) |>
        dplyr::mutate(
            Attribute_value = sub(
                '^(\\-)([0-9]+(\\.[0-9]+)?)', 'minus\\2', Attribute_value
            )
        ) |>
        dplyr::mutate(
            Attribute_value = gsub(' ', '', Attribute_value),
            Attribute_value = dplyr::case_when(
                grepl('<', Attribute_value) ~ paste0('-', Attribute_value),
                grepl('>', Attribute_value) ~ paste0(Attribute_value, '-'),
                !grepl("\\-", Attribute_value) ~
                    paste0(Attribute_value, '-', Attribute_value),
                grepl("^\\-", Attribute_value) ~
                    paste0("minusInf", Attribute_value),
                grepl("\\-$", Attribute_value) ~ paste0(Attribute_value, "Inf"),
                TRUE ~ Attribute_value
            ),
            Attribute_value = sub('(<|>)', '', Attribute_value),
            Attribute_value = dplyr::case_when(
                grepl("^\\-", Attribute_value) ~
                    paste0("minusInf", Attribute_value),
                grepl("\\-$", Attribute_value) ~ paste0(Attribute_value, "Inf"),
                TRUE ~ Attribute_value
            )
        ) |>
        tidyr::separate(
            col = 'Attribute_value',
            into = c('Attribute_value_min', 'Attribute_value_max'), sep = '-'
        ) |>
        dplyr::mutate(
            Attribute_value_min = sub('minus', '-', Attribute_value_min),
            Attribute_value_max = sub('minus', '-', Attribute_value_max)
        ) |>
        dplyr::mutate(
            Attribute_value_min = as.double(Attribute_value_min),
            Attribute_value_max = as.double(Attribute_value_max)
        ) |>
        dplyr::distinct()
}

## helper function for .importSpreadsheets
.discreteAttributeTypes <- function() {
    fname <- system.file(
        'extdata', 'spreadsheet_links.tsv', package = 'bugphyzz'
    )
    dat <- utils::read.table(file = fname, header = TRUE, sep = '\t')
    unique(dat[dat$trait_type == 'discrete',]$attribute_type)
}

## Helper function for physiologies
.addSourceInfo <- function(dat) {
    fpath <- system.file(
        'extdata', 'attribute_sources.tsv', package = 'bugphyzz'
    )
    sourceData <- utils::read.table(
        file = fpath, header = TRUE, sep = '\t', quote = '',
        check.names = FALSE, comment.char = ''
    )
    dplyr::left_join(dat, sourceData, by = 'Attribute_source')
}

## Helper function for physiologies
.reorderColumns <- function(df, name = NULL, attrType) {
    colNames <- colnames(df)
    reqCols <- .requiredColumns(attrType)
    colsLgl <- reqCols %in% colNames
    if (!all(colsLgl)) {
        missingCols <- paste0(reqCols[!colsLgl], collapse = ', ')
        if (!is.null(name)) {
            msg <- paste0(
                'Missing columns in ', name, '.', ' Missing columns are: ',
                missingCols
            )
        } else {
            msg <- paste0(
                'Missing columns.', ' Missing columns are: ', missingCols
            )
        }
        warning(msg, call. = FALSE)
    }
    cols <- reqCols[colsLgl]
    df |>
        dplyr::relocate(dplyr::all_of(cols))
}

## Helper function for physiologies. Maps source terms (BacDive's
## "rod-shaped", the 2020 Bergey's "vibriod cell", ...) to the controlled
## vocabulary in attributes.tsv, using inst/extdata/attribute_synonyms.tsv.
## A source term may map to several attributes (one row each); an empty
## attribute drops the row on purpose. Only rows with Attribute_value TRUE are
## mapped: some mappings are lossy ("aerotolerant" to "anaerobic"), so a FALSE
## row keeps its source term and fails the vocabulary check instead of
## silently changing meaning. Terms not in the table are unchanged.
.applySynonyms <- function(df) {
    if (!all(c('Attribute', 'Attribute_value') %in% colnames(df)))
        return(df)
    syn <- .attributeSynonyms()
    syn <- data.frame(
        Attribute_group = syn$attribute_group, Attribute = syn$source_term,
        .synonym = syn$attribute, .mapTrue = TRUE, check.names = FALSE
    )
    df$.mapTrue <- df$Attribute_value %in% TRUE
    df <- dplyr::left_join(
        df, syn, by = c('Attribute_group', 'Attribute', '.mapTrue'),
        relationship = 'many-to-many'
    )
    df <- df[is.na(df$.synonym) | nzchar(df$.synonym), , drop = FALSE]
    df$Attribute <- dplyr::coalesce(df$.synonym, df$Attribute)
    df$.synonym <- NULL
    df$.mapTrue <- NULL
    dplyr::distinct(df)
}

.attributeSynonyms <- function() {
    fname <- system.file(
        'extdata', 'attribute_synonyms.tsv', package = 'bugphyzz'
    )
    utils::read.table(
        fname, header = TRUE, sep = '\t', quote = '', comment.char = '',
        na.strings = character(0), stringsAsFactors = FALSE
    )
}

## Valid attributes for a physiology, matched on attribute_group the way the
## export (taxPPro::filterDataDiscrete) matches them.
.validAttributes <- function(group) {
    fname <- system.file('extdata', 'attributes.tsv', package = 'bugphyzz')
    df <- utils::read.table(fname, sep = '\t', header = TRUE)
    rgx <- paste0('\\b', group, '\\b')
    unique(df$attribute[grepl(rgx, df$attribute_group)])
}

## Helper function for physiologies
.homogenizeAerophilicityAttributeNames <- function(df) {
    df |> dplyr::mutate(
        Attribute = dplyr::case_when(
            Attribute == 'obligately anaerobic' ~ 'anaerobic',
            Attribute == 'microaerophilic' ~ 'aerobic',
            Attribute == 'obligately aerobic' ~ 'aerobic',
            TRUE ~ Attribute
        )
    )
}

## Required columns
.requiredColumns <- function(attrType) {
    fname <- system.file("extdata/curation_template.tsv", package = "bugphyzz")
    df <- utils::read.table(fname, sep = "\t", header = TRUE)
    lglVct1 <- df$requiredness == "required"
    lglVct2 <- grepl(attrType, df$attribute_types)
    df <- df[lglVct1 & lglVct2,]
    df[order(df[["required_column_order"]]), , drop = FALSE]
    output <- df[['column_name']]
    return(output)
}

## Generate a template for a bugphyzz dataset
.template <- function(dataset) {
    templateTsv <- system.file(
        "extdata/curation_template.tsv", package = "bugphyzz"
    )
    template <- utils::read.table(
        file = templateTsv, sep = "\t", check.names = FALSE, header = TRUE,
        allowEscapes = TRUE )
    # template <- readr::read_tsv(template_tsv, show_col_types = FALSE)
    template[template[["column_name"]] %in% colnames(dataset), ]
}

## Print valid attributes
.attributes <- function() {
    fname <- system.file("extdata/attributes.tsv", package = "bugphyzz")
    df <- utils::read.table(
        fname, sep = "\t", header = TRUE, check.names = FALSE
    )
    unique(df[,"attribute"])
}

## Append links to error table
.appendLinks <- function(x) {
    fname1 <- system.file(
        'extdata/spreadsheet_links.tsv', package = 'bugphyzz'
    )
    links <- utils::read.table(fname1, header = TRUE, sep = '\t')
    selectCols <- c("physiology", "source_link")
    phys_links <- links |>
        dplyr::select(tidyselect::all_of(selectCols))
    custom_links <- .customLinks() |>
        dplyr::select(tidyselect::all_of((selectCols)))
    links <- dplyr::bind_rows(phys_links, custom_links)
    x |>
        dplyr::left_join(links, by = c("dataset" = "physiology"))
}


## Programmatic sources: manifest and files in waldronlab/bugphyzzExports,
## sources/. Each file has the curation-sheet column layout, so it goes
## through the same processing as a Google Sheet.
.sourcesBaseUrl <- function() {
    getOption(
        "bugphyzz.sourcesBaseUrl",
        "https://raw.githubusercontent.com/waldronlab/bugphyzzExports/devel/sources/"
    )
}

.importSources <- function(keyword, forceDownload = FALSE) {
    base <- .sourcesBaseUrl()
    ## cache entries are keyed on the base URL so a local checkout or another
    ## branch never shadows the default location
    tag <- substr(rlang::hash(base), 1, 8)
    mpath <- .getResource(
        paste0("sources_", tag, "_manifest.tsv"), paste0(base, "manifest.tsv"),
        force = forceDownload, verbose = FALSE
    )
    manifest <- utils::read.table(
        mpath, header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE
    )
    manifest <- manifest[manifest$physiology %in% keyword, , drop = FALSE]
    if (!nrow(manifest)) return(list())
    out <- lapply(split(manifest, manifest$physiology), function(m) {
        dplyr::bind_rows(lapply(seq_len(nrow(m)), function(i) {
            fpath <- .getResource(
                paste0("sources_", tag, "_", gsub("/", "_", m$file[i])),
                paste0(base, m$file[i]), force = forceDownload, verbose = FALSE
            )
            df <- utils::read.table(
                fpath, header = TRUE, sep = "\t", quote = "", comment.char = "",
                stringsAsFactors = FALSE, colClasses = "character"
            )
            attrType <- m$attribute_type[i]
            if (attrType %in% .discreteAttributeTypes()) {
                df$Attribute_value <- as.logical(df$Attribute_value)
            } else if (attrType == "numeric") {
                df$Attribute_value <- as.numeric(df$Attribute_value)
            }
            ## range values stay character ("12-43", "<5"); .modifyRange parses them
            .processSheet(df, physName = m$physiology[i], attrType = attrType)
        }))
    })
    out
}
