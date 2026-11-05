## A Zenodo deposit is the GitHub archive of a bugphyzzExports tag, so it also
## carries sources/*.csv.gz and other files. Only the three export tables
## must be read.

test_that(".readZenodoZip reads only the three bugphyzz_*.csv tables", {
    root <- tempfile("deposit")
    dir.create(file.path(root, "sources", "bacdive"), recursive = TRUE)
    tbl <- "# header line\nNCBI_ID,Attribute,Attribute_value\n1,Aerophilicity,aerobic\n"
    for (f in c("binary", "multistate", "numeric"))
        writeLines(tbl, file.path(root, paste0("bugphyzz_", f, ".csv")))
    gz <- gzfile(file.path(root, "sources", "bacdive", "bacdive.csv.gz"), "w")
    writeLines("a,b\n1,2", gz)
    close(gz)
    writeLines("x", file.path(root, "sources", "manifest.tsv"))
    zip <- tempfile(fileext = ".zip")
    withr::with_dir(root, utils::zip(zip, ".", flags = "-rq"))

    out <- bugphyzz:::.readZenodoZip(zip)
    expect_length(out, 3)
    expect_true(all(vapply(out, is.data.frame, logical(1))))
    expect_identical(out[[1]]$Attribute, "aerophilicity")
    expect_false(dir.exists(file.path(tempdir(), "sources")))
})
