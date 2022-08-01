#' Make signatures
#'
#' \code{makeSignatures} converts a bugphyzz dataset into a list of microbial
#' signatures.
#'
#' @param df A data.frame imported with bugphyzz.
#' @param tax_id_type Type of ID. Options: NCBI_ID, Taxon_name.
#' @param tax_level Taxonomy level. Options: superkingdom, kingom, phylum,
#' class, order, family, genus, species, strain, mixed (all taxonomies).
#' Default is mixed.
#' @param evidence Type of evidence of the assigned attribute values to a
#' given taxon. Options: EXP, experimental; ASR, ancestral state reconstruction.
#' Default is both EXP and ASR.
#' @param ci A double indicating how likely is that a given
#' organism has that attribute value. Threshold options: 1, always; 0.8,
#' Usually; 0.5, sometimes; 0.3, Occasionally.
#' Values below 0.3 are not allowed. Default is 0.8.
#' @param include_unknown_ci Include taxa whose attribute value confidence
#' interval is unknown.
#' @param min_size The minimal size of the returned microbial signatures.
#' Default is 1.
#'
#' @return A list of microbial signatures
#'
#' @export
#'
#' @examples
#'
#' aer <- physiologies("aerophilicity")[[1]]
#' aer_signatures <- makeSignatures(aer)
#' lapply(aer, head)
#'
makeSignatures <- function(
  df, tax_id_type = "NCBI_ID", tax_level = "mixed",
  evidence = c("EXP", "ASR", "Unknown"), ci = 0.8,
  include_unknown_ci = TRUE, min_size = 5
) {

  if (!is.data.frame(df)) {
    invalid_class <- class(df)
    msg <- paste0(
      'Input should be a data frame imported from bugphyzz, not an object of',
      ' class ', invalid_class, '.'
    )
    stop(msg, call. = FALSE)
  }

  valid_ranks <- c(
    "superkingdom", "phylum", "class", "order", "family", "genus",
    "species", "strain"
  )

  if (tax_level == "mixed") {
    tax_level <- valid_ranks
  }

  ci_vals <- c(always = 1, usually = 0.8, sometimes = 0.5, occasionally = 0.3)

  if (ci == ci_vals[1]) {
    filter_ci <- names(ci_vals)[1]

  } else if (ci >= ci_vals[2]) {
    filter_ci <- names(ci_vals)[1:2]

  } else if (ci >= ci_vals[3]) {
    filter_ci <- names(ci_vals)[1:3]

  } else if (ci >= ci_vals[4]) {
    filter_ci <- names(ci_vals)[1:4]
  }

  if (include_unknown_ci)
    filter_ci <- c(filter_ci, "Unknown")

  filter_rows <- df$Rank %in% tax_level &
    df$Evidence %in% evidence &
    df$Frequency %in% filter_ci

  select_cols <- c(tax_id_type, "Attribute")

  output <- df[filter_rows, select_cols]

  output <- split(output, factor(output[["Attribute"]]))

  output <- lapply(output, function(x) {
    values <- x[[1]]
    names <- x[[2]]
    names(values) <- names
    unique(values)
  })

  output <-
    output[vapply(output, function(x)  length(x) >= min_size, logical(1))]

  if (!length(output)) {
    message("No signatures found.")
    return(invisible(NULL))
  }
  return(output)
}
