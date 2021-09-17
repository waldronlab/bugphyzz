#' Make signatures
#'
#' \code{makeSignatures} creates a list of microbe signatures from a bugphyzz
#' dataset, i.e. imported with either the \code{\link{physiologies}} or the
#' \code{\link{fattyAcidComposition}} functions.
#'
#' @section Inherited signatures:
#' \code{\link{makeSignatures}} allows the creation of inherited signatures
#' when the `inherited` argument is set to `TRUE`. This means that a taxon of
#' a given taxonomic rank (e.g. species) can inherit an attribute annotation
#' from a taxon of a lower rank (e.g. strain) if they belong to the same
#' lineage. For example, if the Pyrococcus abyssi species is not annotated in
#' a given bugphyzz dataset, but the Pyrococcus furiosus DSM 3638 strain is,
#' then the `inherited = TRUE` argument allows to create a signature at the
#' species level including the Pyrococcus abyssi species.
#'
#' @param df A bugphyzz dataset.
#' @param taxids A character string indicating the type of taxid. Valid
#' options: NCBI_ID, Taxon_name, Genome_ID, Accession_number.
#' @param tax_rank A character string indicating the taxonomic rank of the
#' microbes in the signatures. Valid options: superkingdom, kingdom, phylum,
#' class, order, family, species, strain.
#' @param min_sig_size The minimum number of microbes in a signature.
#' @param inherited If `TRUE` inherited signatures are created. Default
#' is `FALSE`.
#'
#' @return
#' A named list of microbe signatures.
#'
#' @export
#'
#' @examples
#'
#' library(bugphyzz)
#' library(dplyr)
#'
#' ## Make signatures of thermophilic microbes
#'
#' gt <- physiologies("growth temperature")[[1]]
#' gt_therm <- gt %>%
#'     filter(Attribute_value > 80)
#'
#' gt_sig <- makeSignatures(gt_therm, taxids = "NCBI_ID", tax_rank = "species")
#' gt_sig
#'
#' ## Make inherited signatures
#'
#' gt_sig_inherired <- makeSignatures(gt_therm, taxids = "NCBI_ID",
#'                                    tax_rank = "species", inherited = TRUE)
#' gt_sig_inherired
#'
makeSignatures <- function(df, taxids, tax_rank, min_sig_size = 5, inherited = FALSE) {

  valid_ranks <- c("superkingdom", "kingdom", "phylum", "class", "order",
                   "family", "genus", "species", "strain")

  valid_taxids <- c("NCBI_ID", "Taxon_name", "Genome_ID", "Accession_number")

  if (!tax_rank %in% valid_ranks)
    stop("Invalid taxonomy rank. Select one of: ", paste0(valid_ranks, collapse = ", "), ".", call. = FALSE)

  if (!taxids %in% valid_taxids)
    stop("Invalid taxid. Select one of: ", paste0(valid_taxids, collapse = ", "), ".", call. = FALSE)

  annotated_df <- df %>%
    plyr::mutate(NCBI_ID = as.character(as.integer(NCBI_ID))) %>%
    suppressWarnings() %>%
    dplyr::left_join(taxonomyAnnotations, by = "NCBI_ID") %>%
    dplyr::filter(Attribute != FALSE, !is.na(Attribute))

  if (isTRUE(inherited)) {
    if (taxids == "Taxon_name") {
      taxids <- tax_rank
    } else if (taxids == "NCBI_ID") {
      taxids <- paste0(tax_rank, "_id")
    } else {
      stop("Argument taxids can only take 'NCBI_ID' or 'Taxon_name' for inherited signatures.", call. = FALSE)
    }
  } else if (isFALSE(inherited)) {
    annotated_df <- annotated_df %>%
      dplyr::filter(rank == tax_rank)
  }

  annotated_df %>%
    dplyr::select(tidyselect::all_of(c(taxids, "Attribute"))) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    split(f = as.factor(.[["Attribute"]])) %>%
    purrr::map(~ unique(.x[[taxids]])) %>%
    purrr::discard(~ length(.x) < min_sig_size)

}
