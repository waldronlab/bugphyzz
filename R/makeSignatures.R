
getSignatures <- function(
    df, tax.id.type, tax.level = 'mixed',
    Frequency = c('unknown', 'sometimes', 'usually', 'always'),
    Evidence = c('unknown', 'exp', 'igc', 'inh', 'exp'),
    min.size = 1,
    min = NULL, max = NULL,
    num = NULL,
    remove_false = FALSE
) {

  sig_type <- unique(df$Attribute_type)

  if (length(tax.id.type) > 1)
    stop('The tax.id.type argument must be of legth 1.', call. = FALSE)

  if (!tax.id.type %in% c('NCBI_ID', 'Taxon_name'))
    stop('The tax.level argument must be NCBI_ID or Taxon_name', call. = FALSE)

  if (tax.level == 'mixed')
    tax.level <- .validRanks()

  tax_lgl_vct <- tax.level %in% .validRanks()
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
      'No signatures for ', sig_type, '.', ' Returning NULL.', call. = FALSE
    )
    return(NULL)
  }

  if (sig_type == 'logical' || sig_type == 'categorical') {

    if (!is.null(min) || !is.null(max) || !is.null(num))
      warning(
        'min, max, and num arguments are ignored for logical or categorical',
        ' attributes. Check ?getSignatures.',
        call. = FALSE
      )

    df <- df |>
      dplyr::mutate(
        sig_name = paste0(
          'bugphyzz:', Attribute_group, '|', Attribute, '|', Attribute_value
        )
      )

    if (!nrow(df)) {
      warning(
        'No signatures for ', sig_type, '.', ' Returning NULL.', call. = FALSE
      )
      return(NULL)
    }

    split_df <- split(df, factor(df$sig_name))
    sigs <- split_df |>
      purrr::map(~ {
        unique(.x[[tax.id.type]])
      })
    if (remove_false) {
      sigs <- sigs[grepl('TRUE', names(sigs))]
      names(sigs) <- sub('\\|TRUE$', '', names(sigs))
    }
  }

  if (sig_type == 'range') {

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
        'No signatures for ', sig_type, '.', ' Returning NULL.', call. = FALSE
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
    return(NULL)
  }

  sigs

}


## A helper function for makeSignatures
.validRanks <- function() {
  c(
    "superkingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )
}
