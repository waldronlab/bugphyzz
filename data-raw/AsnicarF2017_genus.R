## code to prepare `AsnicarF2017_genus` dataset goes here

filter_genus <- function(path) {

  clade_columns <- c("Kingdom", "Phylum", "Class", "Order",
                     "Family", "Genus", "Species")

  data <- readr::read_tsv(path, comment = "#", col_types = "-cd-",
                   col_names = c("NCBI_ID", "Abundance")) %>%
    dplyr::filter(!is.na(NCBI_ID)) %>%
    tidyr::separate(NCBI_ID, sep = "\\|", into = clade_columns,
             fill = "right") %>%
    dplyr::filter(!is.na(Genus), is.na(Species)) %>%
    dplyr::select(NCBI_ID = Genus, Abundance)
  return(data)

}

tsv_list <- list.files("data-raw/AsnicarF_2017/metaphlan_bugs_list/",
                       full.names = TRUE)

AsnicarF2017_genus <- map(tsv_list, ~filter_genus(.x))
names(AsnicarF2017_genus) <- gsub("\\.tsv", "", basename(tsv_list))

# usethis::use_data(AsnicarF2017_genus, overwrite = TRUE)
