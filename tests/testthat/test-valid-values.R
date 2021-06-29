











#
# database <- physiologies()
#
# is_attr_c <- function(x) {
#   for (i in seq_along(x)) {
#     if ( names(x[i]) == "Attribute" && is.character(x[[i]]) ) {
#       return(x[[i]])
#     }
#   }
# }
#
# test_that("all categorical attribute values are present and valid.", {
#
#   y <- lapply(database, function(x) is_attr_c(x))
#   y <- y[!sapply(y, is.null)]
#   actual_values <- sort(unique(unlist(y, use.names = FALSE)))
#
#   filename <- system.file("extdata", "attributes.tsv",
#                           package = "bugphyzz")
#   df <- read.table(filename, sep = "\t", header = TRUE)
#   valid_values <- sort(unique(df[["attribute"]]))
#
#   expect_identical(actual_values, valid_values)
#
# })
