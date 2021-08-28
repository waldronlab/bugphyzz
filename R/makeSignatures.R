#' Makes taxid signatures for physiologies
#'
#' \code{makeSignatures}  reates a list of microbe signatures from a bugphyzz
#' dataset.
#'
#' @param df A `data.frame` output by the \link{physiologies} function
#' @param taxids identification of the bugs (Taxon name, NCBI ID, etc.)
#' @param taxa_rank Nothing yet
#' @param min An integer. Minimum value. Only applies for attributes with
#' continuous values.
#' @param max An integer. Maximum value. Only applies for attributes with
#' continoues values.
#'
#' @return a large list of character vectors
#' @export
#'
#' @examples
#' aero <- physiologies("aerophilicity")
#' x <- makeSignatures(aero[[1]], taxids = "Taxon_name")
#' lapply(x, head)
#' x <- makeSignatures(aero[[1]], taxids = "NCBI_ID")
#' lapply(x, head)

makeSignatures <- function(df, taxids = "Taxon_name", taxa_rank = "genus", min = NULL, max = NULL) {

  valid_taxids <- c("NCBI_ID", "Genome_ID", "Accession_number", "Taxon_name")

  if (!taxids %in% valid_taxids)
    stop("Invalid taxid type. Valid options:", paste0(valid_taxids, collapse = "|"), call. = FALSE)

  valid_ranks <- c("genus", "species", "strain", "all")

  if (!taxa_rank %in% valid_ranks)
    stop("Invalid rank name. Valid options:", paste0(valid_taxids, collapse = "|"), call. = FALSE)

  taxonomy_table <- .taxonomyTable()

  if (!is.null(min) || !is.null(max)) {

    if (is.null(min))
      min <- min(df[["Attribute_value"]])
    if (is.null(max))
      max <- max(df[["Attribute_value"]])

    df <- df %>%
      dplyr::filter(Attribute_value >= min, Attribute_value <= max)

  } else if (is.null(min) && is.null(max)) {

    df <- df %>%
      dplyr::filter(Attribute_value == TRUE)

  }

  if (taxa_rank == "all") {

    df <- dplyr::left_join(df, taxonomy_table, by = "NCBI_ID") %>%
      dplyr::mutate(rank = ifelse(!rank %in% c("genus", "species", "strain"), "strain", rank))

  } else {

    df <- dplyr::left_join(df, taxonomy_table, by = "NCBI_ID") %>%
      dplyr::filter(rank == taxa_rank) %>%
      dplyr::mutate(rank = ifelse(!rank %in% c("genus", "species", "strain"), "strain", rank))

  }

  attribute_names <- unique(df[["Attribute"]])
  dat <- lapply(attribute_names, fetchBugs, dat = df, taxids = taxids)
  names(dat) <- attribute_names

  if (!length(dat)) {
    NULL
  } else {
    dat
  }
}

#' Fetch bugs based on taxid and corresponding attribute
#'
#' @param attribute_name name of attribute (aerobic, anaerobic, etc.))
#' @param dat a physiology data.frame
#' @param taxids identification of the bugs that will have the attribute value
#'
#' @return a character vector of taxids
#' @export
#'
#' @examples
#' physiology_list <- bugphyzz::physiologies(keyword = "aerophilicity")
#' df <- physiology_list[[1]]
#' x <- fetchBugs(attribute_name = "aerobic", dat = df, taxids = "Taxon_name")
#' head(x)
fetchBugs <- function(attribute_name, dat, taxids = "Taxon_name"){
  bugs <- c(dat[dat[["Attribute"]] == attribute_name,])
  bugs <- unique(bugs[[taxids]])
  return(bugs[!is.na(bugs)])
}

#' Get signatures
#'
#' \code{getSignatures} creates a list of microbe signatures from a bugphyzz
#' dataset. The signatures are created from inherited ranks.
#'
#' @param df A dataset from bugphyzz.
#' @param rank A character string with the taxonomy rank of the signatures.
#' Options: "superkingdom", "kingdom", "phylum", "class", "order", "family",
#' "species", "strain".
#' @param id_type A character string with the type of ID. Options: "Taxon_name", "NCBI_ID", "Accession_number", "Genome_ID".
#' @param min An integer. Minimum value. Only applies for attributes with
#' continuous values.
#' @param max An integer. Maximum value. Only applies for attributes with
#' continoues values.
#'
#' @return
#' A list of microbe signatures
#'
#' @export
#'
#' @examples
#'
#' library(bugphyzz)
#'
#' # Make signatures of bacteria classified by oxygen utilization
#'
#' aer <- physiologies("aerophilicity")[[1]]
#' aer_signatures <- getSignatures(df = aer, rank = "genus", id_type = "name")
#'
#' # Make signature of bacteria growing in extreme temperatures
#'
#' gt <- physiologies("growth temperature")[[1]]
#' gt_signatures <- getSignatures(df = gt, rank = "species", id_type = "name", min = 80)
#'
getSignatures <- function(df, rank = "genus", id_type = "name", min = NULL, max = NULL) {

  valid_ranks <- c("superkingdom", "phylum", "class", "order", "family", "genus", "species", "strain")
  valid_id_types <- c("Taxon_name", "NCBI_ID", "Accession_number", "Genome_ID")

  if (!rank %in% valid_ranks)
    stop(paste0("Invalid 'rank'. Valid options: ", paste0(valid_ranks, collapse = ", "), "."), call. = FALSE)

  if (!id_type %in% valid_id_types)
    stop(paste0("Invalid 'id_type'. Valid options: ", paste0(valid_id_types, collapse = ", "), "."), call. = FALSE)

  rank_id <- paste0(rank, "_id")
  taxonomy_table <- .taxonomyTable()

  if (!is.null(min) || !is.null(max)) {

    if (is.null(min))
      min <- min(df[["Attribute_value"]])
    if (is.null(max))
      max <- max(df[["Attribute_value"]])

    df <- df %>%
      dplyr::filter(Attribute_value >= min, Attribute_value <= max)

  } else if (is.null(min) && is.null(max)) {

    df <- df %>%
      dplyr::filter(Attribute_value == TRUE)

  }

  if (id_type == "Taxon_name") {
    # name is actually obtained from rank, not the Taxon_name column.
    # This is because of possible duplicates (synonyms).
    signatures <- rank
    signatures_names <- rank_id
    select_cols <- c(rank, rank_id, "Attribute")
  } else if (id_type == "NCBI_ID") {
    # name is actually obtained from rank id, not the NCBI_ID column.
    # This is because of possible duplicates (synonyms).
    signatures <- rank_id
    signatures_names <- rank
    select_cols <- c(rank_id, rank, "Attribute")
  } else if (id_type == "Genome_ID" || id_type == "Accession_number") {
    signatures <- id_type
    signatures_names <- rank_id
    select_cols <- c(id_type, rank_id, "Attribute")
  }

  data <- df %>%
    dplyr::left_join(taxonomy_table, by = "NCBI_ID") %>%
    dplyr::mutate(strain = ifelse(!rank %in% c("genus", "species", "strain"), Taxon_name, strain),
                  strain_id = ifelse(!rank %in% c("genus", "species", "strain"), NCBI_ID, strain_id)) %>%
    dplyr::select(tidyselect::all_of(select_cols)) %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

  output <- split(data, data[["Attribute"]]) %>%
    lapply(function(x) {
      taxa <- x[[signatures]]
      names(taxa) <- x[[signatures_names]]
      taxa
    })

  if (!length(output)) {
    NULL
  } else {
    output
  }
}


