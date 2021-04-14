#' Makes taxid signatures for physiologies
#'
#' @param physiologies the desired physiology
#' @param taxids identification of the bugs (Taxon name, NCBI ID, etc.)
#' @param rank Nothing yet
#'
#' @return a large list of character vectors
#' @export
#'
#' @examples
#' x <- make_signatures("aerophilicity")
#' head(x[["aerobic"]])
make_signatures <- function(physiologies = "all", taxids = "Taxon_name", rank = "all"){
  phyzzlist <- bugphyzz::physiologies(keyword = physiologies)
  df <- phyzzlist[[1]]
  attribute_names <- unique(df[["Attribute"]])
  dat <- lapply(attribute_names, fetch_bugs, dat = df, taxids = taxids)
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
#' physiology_list <- bugphyzz::physiologies(keyword = physiologies)
#' df <- physiology_list[[1]]
#' x <- fetchBugs(attribute_name = "aerobic", dat = df, taxids = "Taxon_name")
#' head(x)
fetch_bugs <- function(attribute_name, dat, taxids = "Taxon_name"){
  bugs <- c(dat[dat[["Attribute"]] == attribute_name,])
  bugs <- bugs[[taxids]]
  return(bugs[!is.na(bugs)])
}

