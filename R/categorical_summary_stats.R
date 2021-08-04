#pacman::p_load("dplyr", "ggplot2", "bugphyzz")

#' Display number of taxa with/without NCBI ID in physiology dataset
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' physiologies <- physiologies()
#' aer <- physiologies[["aerophilicity"]] %>%
#' as_tibble()
#' ncbi_stats(aer, "aerophilicity")
#'
ncbi_stats <- function(data, phys = deparse(substitute(data)))
{
  title <- paste("Number of taxa in the",phys, "dataset")
  grepl("^[0-9]+$", data$NCBI_ID) %>%
    as_tibble() %>%
    magrittr::set_colnames("Taxa") %>%
    mutate(Taxa = ifelse(Taxa == TRUE, "Taxa with NCBI ID", "Taxa without NCBI ID")) %>%
    count(Taxa) %>%
    ggplot(aes(Taxa, n)) +
    geom_col(fill = "gray50", color = "black") +
    geom_label(aes(label = n)) +
    labs(title = title,
         x = "Taxa", y = "Number of Taxa") +
    theme_bw() +
    theme(plot.title = element_text(size = 8))
}
#' Fetch ranks for bacteria
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' @return a tibble object
#' @keywords internal
#'
#' @examples
#' physiologies <- physiologies()
#' aer <- physiologies[["aerophilicity"]] %>%
#' as_tibble()
#' fetch_ranks(aer, "aerophilicity")
fetch_ranks <- function(data)
{
  data <- data[!is.na(data$NCBI_ID),]
  ncbi_ranks <- ncbiRank(x = data$NCBI_ID, ranks = c("phylum", "genus", "species"))
  ncbi_ranks$Taxon_name <- data$Taxon_name
  with_ranks <- full_join(ncbi_ranks, data, by = c("Taxon_name", "NCBI_ID"))
  with_ranks <- with_ranks[,-c(6:7,10:12)] %>% unique()
  return(with_ranks)
}

#' Display number of genera per attribute in physiology dataset
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' and has been converted by the \link{fetch_ranks} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' physiologies <- physiologies()
#' aer <- physiologies[["aerophilicity"]] %>%
#' as_tibble()
#' aer_ranks <- fetch_ranks(aer)
#' num_genera_per_attribute(aer_ranks, "aerophilicity")
num_genera_per_attribute <- function(data, phys)
{
  title <- paste("Number of genera per attribute in the", phys, "dataset")
  data %>%
    filter(Attribute_value == TRUE, !is.na(genus)) %>%
    select(Attribute) %>%
    count(Attribute) %>%
    arrange(-n) %>%
    ggplot(aes(reorder(Attribute, -n), n)) +
    geom_col(fill = "gray50", color = "black") +
    geom_label(aes(label = n)) +
    labs(title = title,
         x = "Attributes", y = "Number of genera") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 8)
    )
}

#' Display number of taxa without NCBI ID per attribute in physiology dataset
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' and has been converted by the \link{fetch_ranks} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' physiologies <- physiologies()
#' aer <- physiologies[["aerophilicity"]] %>%
#' as_tibble()
#' aer_ranks <- fetch_ranks(aer)
#' num_taxa_without_ncbi_per_attribute(aer_ranks, "aerophilicity")
num_taxa_without_ncbi_per_attribute <- function(data, phys)
{
  title <- paste("Number of taxa without NCBI ID per attribute in the",
                 phys, "dataset")
  data %>%
    filter(Attribute_value == TRUE, is.na(NCBI_ID)) %>%
    select(Attribute) %>%
    count(Attribute) %>%
    arrange(-n) %>%
    ggplot(aes(reorder(Attribute, -n), n)) +
    geom_col(fill = "gray50", color = "black") +
    geom_label(aes(label = n)) +
    labs(title = title,
         x = "Attributes", y = "Number of genera") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Display number of genera per phylum per attribute in physiology dataset
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' and has been converted by the \link{fetch_ranks} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' physiologies <- physiologies()
#' aer <- physiologies[["aerophilicity"]] %>%
#' as_tibble()
#' aer_ranks <- fetch_ranks(aer)
#' attribute_catalog(aer_ranks, "aerophilicity")
attribute_catalog <- function(data, phys)
{
  title <- paste("Number of genera per phylum per attribute in",
                 phys, "dataset")
  data %>%
    filter(!is.na(genus), is.na(species), Attribute_value == TRUE) %>%
    mutate(Attribute = as.factor(Attribute), phylum = as.factor(phylum)) %>%
    count(Attribute, phylum, .drop = FALSE) %>%
    ggplot(aes(phylum, n)) +
    geom_col(aes(fill = Attribute), position = position_dodge2(width = 0.9, preserve = "single")) +
    facet_wrap(~phylum, scales = "free") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(title = title,
         x = "Phylum", y = "Number of Genera") +
    theme_bw()
}
