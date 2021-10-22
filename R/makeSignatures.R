utils::globalVariables(c("NCBI_ID", "Attribute", "rank", "."))
#' Make signatures
#'
#' \code{makeSignatures} creates a list of microbe signatures from a bugphyzz
#' dataset, i.e., a dataset imported either with the \code{\link{physiologies}}
#' or the \code{\link{fattyAcidComposition}} functions.
#'
#' @section Types of signatures:
#'
#' \code{\link{makeSignatures}} allows the creation of three types of
#' signatures (regular, ranked, or inherited) with the `type` argument.
#'
#' + The `regular` option (default) creates a list of signatures per attribute
#' considering all of the taxids and attributes in the dataset, regardless of
#' taxonomic rank.
#'
#' + The `ranked` option filters the taxids by taxonomic rank before creating
#' the lists of signatures. Currently, only "species", "genus", and "strain"
#' rank levels are supported.
#'
#' + The `inherited` option allows the creation of signatures in which the
#' members of the list inherit an attribute annotation from a taxon of a lower
#' level, as long as they belong to the same lineage. For example, if the
#' *Pyrococcus abyssi* species is not annotated in a given bugphyzz dataset,
#' but the *Pyrococcus furiosus* DSM 3638 strain is, then the
#' `type = "inherited"` argument allows to create a signature at the species
#' level including the *Pyrococcus abyssi* species.
#'
#' @param df A bugphyzz dataset.
#' @param taxids A character string indicating the type of taxid. Valid
#' options: NCBI_ID, Taxon_name, Genome_ID, Accession_ID.
#' @param tax_rank A character string indicating the taxonomic rank of the
#' microbes in the signatures. Valid options: superkingdom, kingdom, phylum,
#' class, order, family, species, strain.
#' @param min_sig_size The minimum number of microbes in a signature.
#' @param sig_type A character string. Valid options: regular, normal, inherited.
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
#' ## Example 1- Crete a list of signatures from the aerophilicity dataset
#' aer <- physiologies("aerophilicity")[[1]]
#' aer_sig <- makeSignatures(aer, taxids = "NCBI_ID", min_sig_size = 5)
#'
#' ## Exampl 2 - Create signatures of thermophilic bacteria
#' gt <- physiologies("growth temperature")[[1]]
#' tm <- gt |> filter(Attribute_value > 80)
#'
#' ## Species level signatures with ranked option
#' tm_sig_ranked <- makeSignatures(tm, taxids = "Taxon_name",
#'                                 tax_rank = "species", sig_type = "ranked")
#'
#' ## Species level signtures with inherited option
#' tm_sig_inherited <- makeSignatures(tm, taxids = "Taxon_name",
#'                                    tax_rank = "species",
#'                                    sig_type = "inherited")
#'
#' ## Compare lengths
#' length(tm_sig_inherited[[1]]) > length(tm_sig_ranked[[1]])
#'
makeSignatures <- function(df, taxids, sig_type = "regular", tax_rank = NULL, min_sig_size = 1) {

  valid_ranks <- c("superkingdom", "kingdom", "phylum", "class", "order",
                   "family", "genus", "species", "strain")

  valid_taxids <- c("NCBI_ID", "Taxon_name", "Genome_ID", "Accession_ID")

  valid_sig_types <- c("regular", "ranked", "inherited")

  if (!taxids %in% valid_taxids)
    stop("Invalid taxid. Select one of: ", paste0(valid_taxids, collapse = ", "), ".", call. = FALSE)

  if (!sig_type %in% valid_sig_types)
    stop("Invalid signature type. Select one of: ", paste0(valid_sig_types, collapse = ", "), ".", call. = FALSE)

  annotated_df <- df %>%
    dplyr::mutate(NCBI_ID = as.character(as.integer(NCBI_ID))) %>%
    suppressWarnings() %>%
    dplyr::left_join(taxonomyAnnotations, by = c("NCBI_ID" = "bugphyzz_NCBI_ID")) %>%
    dplyr::filter(Attribute != FALSE, !is.na(Attribute))

  if (sig_type == "inherited") {

    if (is.null(tax_rank))
      stop("Taxonomic rank not specified. You need to specify one with the `tax_rank` argument to create 'ranked' or 'inherired' signatures.", call. = FALSE)

    if (!is.null(tax_rank) && !tax_rank %in% valid_ranks)
      stop("Invalid taxonomy rank. Select one of: ", paste0(valid_ranks, collapse = ", "), ".", call. = FALSE)

    if (taxids == "Taxon_name") {
      taxids <- paste0("bugphyzz_", tax_rank)
    } else if (taxids == "NCBI_ID") {
      taxids <- paste0("bugphyzz_", tax_rank, "_id")
    } else {
      stop("Argument taxids can only take 'NCBI_ID' or 'Taxon_name' for inherited signatures.", call. = FALSE)
    }

  } else if (sig_type == "ranked") {

    if (is.null(tax_rank))
      stop("Taxonomic rank not specified. You need to specify one with the `tax_rank` argument to create 'ranked' or 'inherired' signatures.", call. = FALSE)

    if (!is.null(tax_rank) && !tax_rank %in% c("genus", "species", "strain"))
      stop("Invalid taxonomy rank with 'sig_type = \"ranked\"' option.",
           " Select one of: genus, species, or strain", call. = FALSE)

    annotated_df <- annotated_df %>%
      dplyr::filter(bugphyzz_rank == tax_rank)

  } else if (sig_type == "regular") {

    if (!is.null(tax_rank))
      warning("Taxonomic rank not required for 'regular' signatures. `tax_rank` argument value (", tax_rank, ") ignored.", call. = FALSE)
  }

  signatures <- annotated_df %>%
    dplyr::select(tidyselect::all_of(c(taxids, "Attribute"))) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    split(f = as.factor(.[["Attribute"]])) %>%
    purrr::map(~ unique(.x[[taxids]])) %>%
    purrr::discard(~ length(.x) < min_sig_size)

  if (!length(signatures)) {
    message("No signatures.")
    return(NULL)
  }

  signatures

}
