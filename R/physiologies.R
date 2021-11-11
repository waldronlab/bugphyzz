#' Fetch physiological data
#'
#' @param keyword a character vector of physiologies desired (see \code{\link{curationLinks}}). For the available
#' physiologies, run bugphyzz::physiologiesList()
#'
#' @return a large list of data.frames
#' @importFrom utils read.table
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' x <- physiologies()
#' head(x[[1]])
#'
#' y <- physiologies(c("gram stain", "aerophilicity"))
#' head(y[[1]])
physiologies <- function(keyword = "all") {

  if (all(keyword != "all") & !all(keyword %in% physiologiesList())) {
    stop("Invalid keyword '", keyword, "'. Valid keywords: \n\n",
         paste0(physiologiesList(), collapse = ", "), call. = FALSE)
  }

  links <- curationLinks(keyword = keyword)[, c("physiology", "link")]

  database <- vector("list", nrow(links))

  for (i in seq_along(database)) {

    names(database)[i] <- links[i, "physiology"]
    database[[i]] <- utils::read.csv(links[i, "link"])

    nmissing <- sum(is.na(database[[i]]$Attribute_value))

    if (nmissing > 0) {
      message("Dropped ", nmissing, " rows with missing Attribute_value from ", names(database)[[i]])
    }else{
      message("Finished ", names(database)[[i]])
    }

    database[[i]] <- database[[i]][!is.na(database[[i]]$Attribute_value), ]

    rp <- ranks_parents
    class(rp[["NCBI_ID"]]) <- class(database[[i]][["NCBI_ID"]])
    database[[i]] <- dplyr::left_join(database[[i]], rp, by = "NCBI_ID")
  }
  return(database)
}

#' Show links to curation spreadsheets
#'
#' @param keyword a character vector of physiologies desired. For the available
#' physiologies, run bugphyzz::physiologiesList(). Use "all" for all available physiologies.
#'
#' @return a data.frame with physiology names and URLs
#' @keywords internal
#'
#' @examples
#' bugphyzz:::curationLinks()
#' bugphyzz:::curationLinks(keyword = "aerophilicity")
#' bugphyzz:::curationLinks(keyword = c("aerophilicity", "gram stain"))
curationLinks <- function(keyword = "all"){
  fname <-
    system.file(file.path("extdata", "links.tsv"), package = "bugphyzz")
  links <- utils::read.table(fname, sep = "\t", header = TRUE)
  ifelse(keyword[1] == "all", links, links <-
           links[links$physiology %in% keyword,])
  return(links)
}
#' List of available physiologies
#'
#' \code{physiologiesList} prints the names of the available datasets provided by
#' the \code{\link{physiologies}} function.
#'
#' @return A character vector with the names of the physiologies datasets.
#' @export
#'
#' @examples
#' x <- physiologiesList()
physiologiesList <- function(){
  curationLinks()[["physiology"]]
}
