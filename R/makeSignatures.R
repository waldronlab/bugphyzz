#' Makes taxid signatures for physiologies
#'
#' @param df A `data.frame` output by the \link{physiologies} function
#' @param taxids identification of the bugs (Taxon name, NCBI ID, etc.)
#' @param rank Nothing yet
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

makeSignatures <- function(df, taxids = "Taxon_name", rank = "all"){
  attribute_names <- unique(df[["Attribute"]])
  dat <- lapply(attribute_names, fetchBugs, dat = df, taxids = taxids)
  names(dat) <- attribute_names
  return(dat)
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

