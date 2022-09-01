
#' Make signatures
#'
#' \code{makeSignatures} creates a list of signatures from a bugphyzz
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
#' aer_sig <- makeSignatures(aer)
#'
#' ph <- physiologies('optimal ph')[[1]]
#' ph_sig <- makeSignatures(ph)
#'
#'
makeSignatures <- function(
    df,
    tax.id.type = 'Taxon_name', tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('asr', 'unknown', 'igc', 'asr'),
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
  Rank <- Frequency <- Evidence <- NULL
  output <- df |>
    dplyr::filter(
      Rank %in% tax.level,
      Frequency %in% Frequency,
      Evidence %in% Evidence
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
      Rank %in% tax.level,
      Frequency %in% Frequency,
      Evidence %in% Evidence,
      Attribute_value >= min, Attribute_value <= max
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
#' argument of the \code[makeSignatures] function.
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

# Make signatures
#
# \code{makeSignatures} converts a bugphyzz dataset into a list of microbial
# signatures.
#
# @param df A data.frame imported with bugphyzz.
# @param tax_id_type Type of ID. Options: NCBI_ID, Taxon_name.
# @param tax_level Taxonomy level. Options: superkingdom, kingom, phylum,
# class, order, family, genus, species, strain, mixed (all taxonomies).
# Default is mixed.
# @param evidence Type of evidence of the assigned attribute values to a
# given taxon. Options: EXP, experimental; ASR, ancestral state reconstruction.
# Default is both EXP and ASR.
# @param ci A double indicating how likely is that a given
# organism has that attribute value. Threshold options: 1, always; 0.8,
# Usually; 0.5, sometimes; 0.3, Occasionally.
# Values below 0.3 are not allowed. Default is 0.8.
# @param include_unknown_ci Include taxa whose attribute value confidence
# interval is unknown.
# @param min_size The minimal size of the returned microbial signatures.
# Default is 1.
#
# @return A list of microbial signatures
#
# @export
#
# @examples
#
# aer <- physiologies("aerophilicity")[[1]]
# aer_signatures <- makeSignatures(aer)
# lapply(aer, head)
#
# makeSignatures <- function(
    #   df, tax_id_type = "NCBI_ID", tax_level = "mixed",
#   evidence = c("EXP", "ASR", "Unknown"), ci = 0.8,
#   include_unknown_ci = TRUE, min_size = 5
# ) {
#
#   if (!is.data.frame(df)) {
#     invalid_class <- class(df)
#     msg <- paste0(
#       'Input should be a data frame imported from bugphyzz, not an object of',
#       ' class ', invalid_class, '.'
#     )
#     stop(msg, call. = FALSE)
#   }
#
#   valid_ranks <- c(
#     "superkingdom", "phylum", "class", "order", "family", "genus",
#     "species", "strain"
#   )
#
#   if (tax_level == "mixed") {
#     tax_level <- valid_ranks
#   }
#
#   ci_vals <- c(always = 1, usually = 0.8, sometimes = 0.5, occasionally = 0.3)
#
#   if (ci == ci_vals[1]) {
#     filter_ci <- names(ci_vals)[1]
#
#   } else if (ci >= ci_vals[2]) {
#     filter_ci <- names(ci_vals)[1:2]
#
#   } else if (ci >= ci_vals[3]) {
#     filter_ci <- names(ci_vals)[1:3]
#
#   } else if (ci >= ci_vals[4]) {
#     filter_ci <- names(ci_vals)[1:4]
#   }
#
#   if (include_unknown_ci)
#     filter_ci <- c(filter_ci, "Unknown")
#
#   filter_rows <- df$Rank %in% tax_level &
#     df$Evidence %in% evidence &
#     df$Frequency %in% filter_ci
#
#   select_cols <- c(tax_id_type, "Attribute")
#
#   output <- df[filter_rows, select_cols]
#
#   output <- split(output, factor(output[["Attribute"]]))
#
#   output <- lapply(output, function(x) {
#     values <- x[[1]]
#     names <- x[[2]]
#     names(values) <- names
#     unique(values)
#   })
#
#   output <-
#     output[vapply(output, function(x)  length(x) >= min_size, logical(1))]
#
#   if (!length(output)) {
#     message("No signatures found.")
#     return(invisible(NULL))
#   }
#   return(output)
# }

