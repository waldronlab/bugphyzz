#' Display frequency
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' physiologies <- physiologies()
#' gt <- physiologies[["growth temperature"]] %>%
#' as_tibble()
#' display_freq(gt, "growth temperature")
#'}
display_freq <- function(data, phys)
{
  data %>%
    ggplot(aes(Attribute_value)) +
    geom_histogram(bins = 30, color = "black", fill = "gray60", stat = "count") +
    labs(x = phys, y = "Frequency") +
    theme_bw()
}
#' Display boxplot per phylum
#'
#' @param data a tibble object, converted from the output by the \link{physiologies} function
#' @param phys a string vector of the physiology name
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' physiologies <- physiologies()
#' gt <- physiologies[["growth temperature"]] %>%
#' as_tibble()
#' boxplot_per_phylum(gt, "growth temperature")
#'}
boxplot_per_phylum <- function(data, phys, unit = "unit")
{
  title <- paste(phys, "per phylum")
  y <- paste(phys, "(", unit, ")")
  data %>%
    ggplot(aes(phylum, Attribute_value)) +
    geom_boxplot() +
    labs(title = title, y = y,
         x = "Phylum") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

