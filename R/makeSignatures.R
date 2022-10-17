utils::globalVariables(c(
  '.data', '.env'
))
#' Make signatures
#'
#' \code{makeSignatures} creates a list of signatures.
#'
#' @param keyword A valid keyword as determined by `physiologiesList()`.
#' @inheritParams getSignatures
#'
#' @return A list of signatures. NULL if no signatures.
#' @export
#'
#' @seealso
#' \code{\link{getSignatures}}
#'
#' @examples
#' aer_sig <- makeSignatures('aerophilicity')
#' ph_sig <- makeSignatures('optimal ph')
#'
makeSignatures <- function(
    keyword = NULL,
    tax.id.type = 'Taxon_name', tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('asr', 'unknown', 'igc', 'inh-tax'),
    min.size = 1,
    min = NULL, max = NULL
) {

  if (is.null(keyword)) {
    msg <- paste0(
      'You need to provide a keyword.',
      ' "all" for importing all signatures. Or ',
      ' Valid keyword options: ', paste0(physiologiesList(), collapse = ', ')
    )
    stop(msg, call. = FALSE)
  }

  message('Importing spreadsheet(s)...')
  phys <- physiologies(keyword = keyword)

  output <- lapply(phys, function(.x) {
    getSignatures(
      df = .x,
      tax.id.type = tax.id.type, tax.level = tax.level,
      Frequency = Frequency,
      Evidence = Evidence,
      min.size = min.size,
      min = min, max = min
    )
  }) |>
    purrr::discard(is.null)

  # output <- vector('list', length(phys))
  # names(output) <- names(phys)
  # for (i in seq_along(output)) {
  #   message('Dataset:', names(output)[i])
  #   output[[i]] <- getSignatures(
  #     df = phys[[i]],
  #     tax.id.type = tax.id.type, tax.level = tax.level,
  #     Frequency = Frequency,
  #     Evidence = Evidence,
  #     min.size = min.size,
  #     min = min, max = min
  #   )
  # }

  if (!length(output)) {
    message('No signatures')
    return(NULL)
  }

  return(output)
}

#' Get signatures
#'
#' \code{getSignatures} creates a list of signatures from a bugphyzz
#' data frame.
#'
#' @param df A data frame from bugphyzz.
#' @param tax.id.type Type of taxids. Check types with the `whichID` function.
#' @param tax.level A character vector with taxonomic ranks,
#' e.g., genus, species, strain.
#' @param Frequency A character vector with one or more values of: unknown,
#' sometimes, usually, always.
#' @param Evidence A character vector with one or more values of: unknown,
#' sometimes, usually, always.
#' @param min.size The minimun number of elements in a signatures.
#' Default is 1.
#' @param min Minimum threshold for attributes with continuous values. It's
#' ignored when creating signatures for categorical attributes.
#' @param max Maximum threshold for attributes with continuous values. It's
#' ignored when creating signatures for categorical attributes.
#'
#' @return A list of signatures. NULL if no signatures.
#' @export
#'
#' @examples
#'
#' aer <- physiologies('aerophilicity')[[1]]
#' aer_sig <- getSignatures(aer)
#'
#' ph <- physiologies('optimal ph')[[1]]
#' ph_sig <- getSignatures(ph)
#'
#'
getSignatures <- function(
    df,
    tax.id.type = 'Taxon_name', tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('asr', 'unknown', 'igc', 'inh-tax'),
    min.size = 1,
    min = NULL, max = NULL
) {


  if (length(tax.id.type) > 1)
    stop('The tax.id.type argument must be of legth 1.', call. = FALSE)

  if (!tax.id.type %in% whichID(df)) {
    stop(
      'The tax.level argument must be one of ',
      paste0(whichID(df), collapse = ", "), call. = FALSE
      )
  }

  ## Drop false values
  ## We might not want to remove FALS values depending on their meaning and NA
  false_lgl <- df$Attribute_value == FALSE
  n_false <- sum(false_lgl)
  if (n_false > 0) {
    warning('Dropping ', n_false, ' rows with FALSE values.\n', call. = FALSE)
    df <- df[!false_lgl, ]
  }

  ## Filter by tax.level (Rank)
  if ('mixed' %in% tax.level) {
    message('Retrieving all levels of taxa.')
    tax.level <- .valid_ranks()
  }

  ## TODO Code for filtering and propagation must be inserted here

  ## Filtering will depend on type of the Attribute_value column
  if (is.logical(df$Attribute_value)) {

    if (!is.null(min)) {
      warning(
        'Ignoring argument min for categorical attributes.', call. = FALSE
      )
    }

    if (!is.null(max)) {
      warning(
        'Ignoring argument max for categorical attributes.', call. = FALSE
      )
    }

    filtered_df <- .filterCategorical(
      df = df, tax.level = tax.level, Evidence = Evidence,
      Frequency = Frequency
    )
  } else if (is.numeric(df$Attribute_value)) {
    filtered_df <- .filterContinuous(
      df = df, tax.level = tax.level, Evidence = Evidence,
      Frequency = Frequency, min = min, max = max
    )
  } else {
    filtered_df <- NULL
  }

  if (is.null(filtered_df)) {
    ## If not enough microbes for signatures then return NULL
    ## message is already included in any of the filtering functions above
    return(NULL)
  }

  sig_list <- filtered_df |>
    split(factor(filtered_df$Attribute)) |>
    lapply(function(x) {
      unique(x[[tax.id.type]])
    })

  sig_counts <- vapply(sig_list, length, integer(1))
  vct_lgl <- sig_counts >= min.size

  if (sum(vct_lgl) == 0) {
    warning(
      "Not enough elements to create signatures.\n", call. = FALSE
    )
    return(NULL)
  }

  sig_list[vct_lgl]
}

## A helper function for makeSignatures
.valid_ranks <- function() {
  c(
    "superkingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )
}

## A helper function for makeSignatures
.filterCategorical <- function(df, tax.level, Frequency, Evidence) {
  message('Creating signature for categorical attributes.')
  output <- df |>
    dplyr::filter(
      .data[['Rank']] %in% .env[['tax.level']],
      .data[['Frequency']] %in% .env[['Frequency']],
      .data[['Evidence']] %in% .env[['Evidence']]
    )

  if (!nrow(output)) {
    message(
      "Not enough microbes to create signature."
    )
    return(NULL)
  }
  output
}

## A helper function for makeSignatures
.filterContinuous <- function(df, tax.level, Frequency, Evidence, min, max) {
  message('Creating signature for continuous attribute.')
  Rank <- Frequency <- Evidence <- Attribute_value <- NULL
  if (is.null(min)) {
    min = min(df$Attribute_value)
  }

  if (is.null(max)) {
    max = max(df$Attribute_value)
  }

  output <- df |>
    dplyr::filter(
      .data[['Rank']] %in% .env[['tax.level']],
      .data[['Frequency']] %in% .env[['Frequency']],
      .data[['Evidence']] %in% .env[['Evidence']],
      .data[['Attribute_value']] >= .env[['min']],
      .data[['Attribute_value']] <= .env[['max']]
    )

  if (!nrow(output)) {
    message(
      "Not enough microbes to create signature."
    )
    return(NULL)
  }
  output
}


#' Which IDs
#'
#' \code{whichID} gives valid values for the tax.id.type
#' argument of the \code{makeSignatures} function.
#'
#' @param df A data frame from bugphyzz.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' aer <- physiologies('aerophilicity')[[1]]
#' whichID(aer)
whichID <- function(df) {
  ID_cols <- c('Taxon_name', colnames(df)[grepl('ID$', colnames(df))])
  ID_cols[!ID_cols %in% c('Parent_NCBI_ID')]
}

