## code to prepare `AsnicarF2017_genus` dataset goes here

tsv_list <- list.files("data-raw/AsnicarF_2017/metaphlan_bugs_list/",
                       full.names = TRUE)

dfs <- purrr::map(tsv_list, ~readr::read_tsv(.x, comment = '#', col_names = FALSE)) %>% 
purrr::map( 
    function(x) {
        colnames(x)[1:3] <- c("TAXA", "NCBI_ID", "ABUNDANCE")
        x <- x %>% 
            dplyr::select(-X4)
        x})
dfs <- purrr::map2(dfs, basename(paths),
    function(x, y) {
    colnames(x)[3] <- gsub("\\.tsv$", "", y)
    x
})

format_table <- function(x) {
    col_nam <- c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", 
                 "GENUS", "SPECIES")
    output <- x %>% 
        dplyr::separate(TAXA, col_nam, sep = "\\|\\w__") %>% 
        dplyr::mutate(KINGDOM = stringr::str_remove(KINGDOM, "k__")) %>% 
        dplyr::separate(NCBI_ID, paste0("NCBI_ID_", col_nam), sep = "\\|")
    return(output)
}

new_tables <- purrr::map(dfs, format_table)

get_genus_scores <- function(x) {
    output <- x %>% 
    dplyr::filter(!is.na(GENUS), is.na(SPECIES)) %>% 
    dplyr::select(NCBI_ID = NCBI_ID_GENUS, tidyselect::last_col()) %>% 
    tidyr::drop_na()
    return(output)
}


AsnicarF2017_genus <- purrr::map(new_tables, get_genus_scores) %>% 
    purrr::reduce(dplyr::full_join)
AsnicarF2017_genus[is.na(AsnicarF2017_genus)] <- 0

# usethis::use_data(AsnicarF_2017_genus, overwrite = TRUE)
