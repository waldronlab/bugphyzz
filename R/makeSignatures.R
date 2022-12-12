
#' Make signatures
#'
#' \code{makeSignatures} is a wrapper of \code{physiologies} followed by
#' \code{getSignatures}.
#'
#' @inheritParams physiologies
#' @inheritParams getSignatures
#' @param remove_false Whether include the creation of signatures with FALSE
#' values or not. Default is TRUE, i.e., FALSE values are not included.
#'
#' @return List of signatures
#' @export
#'
#' @examples
#'
#' bugphyzz_sigs <- makeSignatures('all', 'Taxon_name')
#'
#'
makeSignatures <- function(
    keyword, tax.id.type, tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('unknown', 'exp', 'igc', 'inh', 'exp'),
    min.size = 1,
    min = NULL, max = NULL,
    num = NULL,
    remove_false = FALSE
) {

  message('>>> Importing dataset(s) <<<')
  message('')
  list_of_df <- physiologies(keyword, remove_false = FALSE)

  message('')
  message('>>> Creating signatures <<<')
  message('')
  signatures <- list_of_df |>
    purrr::map(~{
    getSignatures(
      df = .x,
      tax.id.type = tax.id.type, tax.level = tax.level, Frequency = Frequency,
      Evidence = Evidence, min.size = min.size, min = min, max = max,
      num = num, remove_false = remove_false
    )
  })
  purrr::flatten(signatures)
}

#' Get signatures
#'
#' \code{getSignatures} get signatures from a bugphyzz data frame imported with
#' the \code{physiologies} function.
#'
#' @param df A data frame.
#' @param tax.id.type A character string. NCBI_ID or Taxon_name.
#' @param tax.level A taxonomy level. Check valid ranks with `validRanks()`.
#' Default is 'mixed', which uses all valid ranks.
#' @param Frequency A character string. Possible values are 'always',
#' 'sometimes', 'usually', 'unknown'.
#' @param Evidence A character string. Possible values are 'exp', 'igc', 'inh',
#' 'asr', 'unknown'.
#' @param min.size Minimum size of the signatures. Default is 1.
#' @param min Minimum for range and numeric attributes.
#' @param max Maximum for range and numeric attributes.
#' @param num Exact number for numeric attributes.
#' @param remove_false Whether include the creation of signatures with FALSE
#' values or not. Default is TRUE, i.e., FALSE values are not included.
#'
#' @return A list of signatures (taxa)
#' @export
#'
getSignatures <- function(
    df, tax.id.type, tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('unknown', 'exp', 'igc', 'inh', 'exp'),
    min.size = 1,
    min = NULL, max = NULL,
    num = NULL,
    remove_false = TRUE
) {

  sig_type <- unique(df$Attribute_type)

  if (missing(tax.id.type))
    stop(
      'Need tax.id.type argument. Either NCBI_ID or Taxon_name.',
      call. = FALSE
    )

  if (length(tax.id.type) > 1)
    stop('The tax.id.type argument must be of legth 1.', call. = FALSE)

  if (!tax.id.type %in% c('NCBI_ID', 'Taxon_name'))
    stop('The tax.level argument must be NCBI_ID or Taxon_name', call. = FALSE)

  if (tax.level == 'mixed')
    tax.level <- validRanks()

  tax_lgl_vct <- tax.level %in% validRanks()
  if (!any(tax_lgl_vct)) {
    invalid_ranks <- tax.level[tax_lgl_vct]
    stop(
      'Invalid tax.id.level value(s): ',
      paste0(invalid_ranks, collapse = ', '), call. = FALSE
    )
  }

  df <- df[!is.na(df[[tax.id.type]]) & df[[tax.id.type]] != 'unknown',]
  df <- df |>
    dplyr::filter(
      .data$Rank %in% tax.level,
      .data$Evidence %in% .env$Evidence,
      .data$Frequency %in% .env$Frequency
    )

  if (!nrow(df)) {
    warning(
      'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
      ' Returning NULL.', call. = FALSE
    )
    return(NULL)
  }

  if (sig_type == 'logical' || sig_type == 'categorical') {

    ## TODO maybe convert this if statement into a separate function

    if (!is.null(min) || !is.null(max) || !is.null(num))
      warning(
        'min, max, and num arguments are ignored for logical or categorical',
        ' attributes. Check ?getSignatures.',
        call. = FALSE
      )

    attr_ <- unique(df$Attribute)
    attr_grp_ <- unique(df$Attribute_group)

    if (length(attr_) == 1 && attr_ == attr_grp_) {
      df <- df |>
        dplyr::mutate(
          sig_name = paste0(
            'bugphyzz:', Attribute, '|', Attribute_value
          )
        )
    } else {
      df <- df |>
        dplyr::mutate(
          sig_name = paste0(
            'bugphyzz:', Attribute_group, '|', Attribute, '|', Attribute_value
          )
        )
    }

    if (!nrow(df)) {
      warning(
        'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
        ' Returning NULL.', call. = FALSE
      )
      return(NULL)
    }

    split_df <- split(df, factor(df$sig_name))
    sigs <- split_df |>
      purrr::map(~ {
        unique(.x[[tax.id.type]])
      })

    if (remove_false) {
      sigs <- sigs[!grepl('FALSE', names(sigs))]
      names(sigs) <- sub('\\|TRUE$', '', names(sigs))
    }

    if (!length(sigs)) {
      warning(
        'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
        ' Returning NULL.', call. = FALSE
      )
      return(NULL)
    }

  }

  if (sig_type == 'range') {

    ## Maybe convert this if statement into a separate function

    if (is.null(min)) {
      min <- min(df$Attribute_value_min)
    }

    if (is.null(max)) {
      max <- max(df$Attribute_value_max)
    }

    df <- df |>
      dplyr::mutate(
        sig_name = paste0(
          'bugphyzz:', Attribute, '|', min, '-', max
        )
      ) |>
      dplyr::filter(
        .data$Attribute_value_min >= min, .data$Attribute_value_max <= max
      )

    if (!nrow(df)) {
      warning(
        'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
        ' Returning NULL.', call. = FALSE
      )
      return(NULL)
    }

    split_df <- split(df, factor(df$sig_name))
    sigs <- split_df |>
      purrr::map(~ {
        unique(.x[[tax.id.type]])
      })
  }

  if (sig_type == 'numeric') {

    if ((!is.null(min) || !is.null(max)) && !is.null(num))
      stop(
        'The num argument cannot be used with min and/or max arguments.',
        call. = FALSE
      )

    if (!is.null(num)) {
      df <- df |>
        dplyr::mutate(
          sig_name = paste0('bugphyzz:', Attribute, '|', num)
        ) |>
        dplyr::filter(Attribute_value == num)

      if (!nrow(df)) {
        warning(
          'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
          ' Returning NULL.', call. = FALSE
        )
        return(NULL)
      }

      split_df <- split(df, factor(df$sig_name))
      sigs <- split_df |>
        purrr::map(~ {
          unique(.x[[tax.id.type]])
        })

    } else {

      if (is.null(min)) {
        min <- min(df$Attribute_value)
      }
      if (is.null(max)) {
        max <- max(df$Attribute_value)
      }

      df <- df |>
        dplyr::mutate(
          sig_name = paste0('bugphyzz:', Attribute, '|', min, '-', max)
        ) |>
        dplyr::filter(
          .data$Attribute_value >= min & .data$Attribute_value <= max
        )

      if (!nrow(df)) {
        warning(
          'No signatures for ', unique(df$Attribute_group), ' (', sig_type, ').',
          ' Returning NULL.', call. = FALSE
        )
        return(NULL)
      }

      split_df <- split(df, factor(df$sig_name))
      sigs <- split_df |>
        purrr::map(~ {
          unique(.x[[tax.id.type]])
        })
    }
  }
  sigs |>
    purrr::keep(~ length(.x) >= min.size)
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
    "superkingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )
}
