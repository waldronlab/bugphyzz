#' Function returns tokens
#'
#' @keywords internal
#'
.tokens <- function(df){
    tibble_dat <- tibble::as_tibble(df)
    tibble_dat$abstract <- gsub("-", "", tibble_dat$abstract)

    tibble_dat <- tibble_dat %>%
        tidytext::unnest_tokens(
            output = .data[["word"]], input = .data[["abstract"]],
            strip_punct = T
        ) %>%
        dplyr::anti_join(.data[["stop_words"]], by = "word") %>%
        dplyr::filter(stringr::str_detect(.data[["word"]], "[:alpha:]")) %>%
        dplyr::distinct()
    return(tibble_dat)
}

#' Function replaces synonyms with standard title
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' temp <- bugphyzz:::.standardize(
#'     dat, "gram", words[[1]], colnames(words)[[1]]
#' )
#'
#' }
#'
.standardize <- function(df, standard, synonyms = standard){
    regString <- paste("\\b", standard, "\\b", sep = "")
    df[[3]] <- gsub("[[:punct:][:blank:]]+", " ", df[[3]])
    df[[3]] <- tolower(df[[3]])

    for (i in 1:length(synonyms)) {
        df[[3]] <- gsub(synonyms[i], standard, df[[3]])
    }

    df <- .tokens(df) %>%
        dplyr::filter(stringr::str_detect(.data[["word"]], regString))
    return(df)
}

#' Function modifies the bergey's df and substitutes words
#'
#' @keywords internal
#'
.sub_word <- function(df, standard, synonyms = standard){
    df[[3]] <- gsub("[[:punct:][:blank:]]+", " ", df[[3]])
    df[[3]] <- tolower(df[[3]])
    for (i in 1:length(synonyms)){
        df[[3]] <- gsub(synonyms[i], standard, df[[3]])
    }
    return(df)
}

#' Function retrieves bacteria that need manual curation
#'
#' Also writes to csv
#'
#' @keywords internal
#'
.extractDuplicate <- function(df){
    df <- df[duplicated(df$genus) | duplicated(df$genus, fromLast = TRUE),]
    df <- df[order(df$genus)]
    return(df)
}

#' Extract accurate
#'
#' @keywords internal
#'
.extract_accurate <- function(df){
    df <- df[!(duplicated(df$genus) | duplicated(df$genus, fromLast = TRUE)),]
    df <- df[order(df$genus)]
    return(df)
}
