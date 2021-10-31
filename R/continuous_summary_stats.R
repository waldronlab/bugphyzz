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
# display_freq <- function(data, phys) {
#     data %>%
#         ggplot2::ggplot(ggplot2::aes(Attribute_value)) +
#         ggplot2::geom_histogram(
#             bins = 30, color = "black", fill = "gray60", stat = "count"
#         ) +
#         ggplot2::labs(x = phys, y = "Frequency") +
#         ggplot2::theme_bw()
# }
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
# boxplot_per_phylum <- function(data, phys, unit = "unit") {
#     title <- paste(phys, "per phylum")
#     y <- paste(phys, "(", unit, ")")
#     data %>%
#        ggplot2::ggplot(ggplot2::aes(phylum, Attribute_value)) +
#        ggplot2::geom_boxplot() +
#        ggplot2::labs(title = title, y = y, x = "Phylum") +
#        ggplot2::theme_bw() +
#        ggplot2::theme(
#          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
#        )
# }

