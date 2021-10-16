reqCols <- function() {
    fname <- system.file("extdata/template.tsv", package = "bugphyzz")
    df <- utils::read.table(fname, sep = "\t", header = TRUE)
    df <- df[df[["requiredness"]] == "required",]
    df[order(df[["required_column_order"]]), "column_name"]
}
