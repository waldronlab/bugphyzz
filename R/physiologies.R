#' Fetch physiological data
#'
#' @param keyword a character vector of physiologies desired (see \code{\link{curationLinks}}). For the available
#' physiologies, run bugphyzz::showPhys()
#'
#' @return a large list of data.frames
#' @importFrom utils read.table
#' @importFrom utils read.csv
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @importFrom purrr modify_if
#' @importFrom purrr modify_at
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_lower
#'
#' @export
#'
#' @examples
#' x <- physiologies()
#' head(x[[1]])
#'
#' y <- physiologies(c("gram stain", "aerophilicity"))
#' lapply(y, head)
physiologies <- function(keyword = "all") {

  if (all(keyword != "all") & !all(keyword %in% showPhys())) {
    stop("Invalid keyword '", keyword, "'. Valid keywords: \n\n",
         paste0(showPhys(), collapse = ", "), call. = FALSE)
  }

  links <- curationLinks(keyword = keyword)[, c("physiology", "link")]
  database <- vector("list", nrow(links))
  names(database) <- links[['physiology']]
  for (i in seq_along(database)) {

    database[[i]] <- dplyr::distinct(utils::read.csv(links[i, "link"]))

    ## Drop missing values from the Attribute_value column
    nmissing <- sum(is.na(database[[i]]$Attribute_value))
    if (nmissing > 0) {
      message(
        "Dropped ", nmissing, " rows with missing Attribute_value from ",
        names(database)[[i]]
      )
    }else{
      message("Finished ", names(database)[[i]])
    }
    database[[i]] <- database[[i]][!is.na(database[[i]]$Attribute_value), ]

    ## Add rank and parent information for taxa with NCBI_ID

    col_names <- colnames(database[[i]])
    parent_col_names <- c('Parent_name', 'Parent_NCBI_ID', 'Parent_rank')
    if (all(parent_col_names %in% col_names)) {
      database[[i]][["NCBI_ID"]] <- as.character(database[[i]][["NCBI_ID"]])
    } else {
      rp <- ranks_parents
      class(rp[["NCBI_ID"]]) <- class(database[[i]][["NCBI_ID"]])
      database[[i]] <- dplyr::left_join(database[[i]], rp, by = "NCBI_ID")
    }

    ## Some modifications for the curation of the datasets
    database[[i]] <- database[[i]] |>
      purrr::modify_if(.p = is.character, ~ stringr::str_squish(.x)) |>
      .addConfidenceInCuration() |>
      purrr::modify_at(.at = c('Frequency', 'Evidence', 'Confidence_in_curation'), ~ {
        stringr::str_to_lower(.x)
      }) |>
      dplyr::distinct()

    database[[i]] <- .reorderColumns(database[[i]], names(database)[i])
  }



  return(database)
}

#' Show links to curation spreadsheets
#'
#' @param keyword a character vector of physiologies desired. For the available
#' physiologies, run bugphyzz::showPhys(). Use "all" for all available physiologies.
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
#' \code{showPhys} prints the names of the available datasets provided by
#' the \code{\link{physiologies}} function.
#'
#' @return A character vector with the names of the physiologies datasets.
#' @export
#'
#' @examples
#' x <- showPhys()
showPhys <- function(){
  curationLinks()[["physiology"]]
}

## A function to reorder columns on import
.reorderColumns <- function(df, name = NULL) {
  col_names <- colnames(df)
  req_cols <- .requiredColumns()

  cols_lgl <- req_cols %in% col_names

  if (!all(cols_lgl)) {

    missing_cols <- paste0(req_cols[!cols_lgl], collapse = ', ')

    if (!is.null(name)) {
      msg <- paste0(
        'Missing columns in ', name, '.', ' Missing columns are: ',
        missing_cols
      )
    } else {
      msg <- paste0(
        'Missing columns.', ' Missing columns are: ', missing_cols
      )
    }
      warning(msg, call. = FALSE)
  }

  cols <- req_cols[cols_lgl]
  df |>
    dplyr::relocate(dplyr::all_of(cols))
}

## Import confidece in curation
.addConfidenceInCuration <- function(df) {
  fpath <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTRiwYhaVaBhGkHQrjRMHJglc9cqxjA90GLsIxCQa91VwZQ65qRQn2wZgwpw3uAOBSMlwFgOPae8PYq/pub?gid=0&single=true&output=tsv'
  fpath <- system.file(
    'extdata/confidence_in_curation.tsv', package = 'bugphyzz'
  )
  cc <- readr::read_tsv(fpath, show_col_types = FALSE)
  dplyr::left_join(df, cc, by = 'Attribute_source')
}
