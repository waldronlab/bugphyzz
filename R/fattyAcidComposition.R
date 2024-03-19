
## Function for importing fatty acid compositions
## TODO This dataset needs more curation.
## TODO Names of the Fatty Acids should be more "user-friendly"
## TODO Maybe a threshold should be decided to consider a FA as present or not.
.fattyAcidComposition <- function(){
  link <- .customLinks() |>
    dplyr::filter(.data[["functionname"]] == "fattyAcidComposition") |>
    dplyr::pull(.data[["link"]])
  fac_wide <- utils::read.csv(link, check.names = FALSE)
  fac_long <- fac_wide |>
    tidyr::pivot_longer(
      cols = .data[["Br-C10:1"]]:.data[["Oxo-C19:1"]],
      names_to = "Attribute_new", values_to = "Attribute_value"
  ) |>
    dplyr::mutate(NCBI_ID = as.character(.data[["NCBI_ID"]]))
  dplyr::left_join(fac_long, ranks_parents, by = "NCBI_ID") |>
    as.data.frame() |>
    .addSourceInfo() |>
    purrr::modify_at(
      .at = c('Attribute', 'Frequency', 'Evidence', 'Confidence_in_curation'),
      .f = ~ stringr::str_to_lower(.x)
    ) |>
    dplyr::select(-Attribute) |>
    dplyr::rename(Attribute = Attribute_new) |>
    .reorderColumns(attr_type = 'numeric')
}

## Function to import custom links
.customLinks <- function(keyword = "all"){
    fname <-
        system.file("extdata/spreadsheet_customlinks.tsv", package = "bugphyzz")
    links <- utils::read.table(fname, sep = '\t', header = TRUE)
    ifelse(keyword[1] == "all", links, links <-
                      links[links$physiology %in% keyword,])
    links
}
