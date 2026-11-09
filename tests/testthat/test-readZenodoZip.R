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
    expect_length(out$tables, 3)
    expect_true(all(vapply(out$tables, is.data.frame, logical(1))))
    expect_identical(out$tables[[1]]$Attribute, "aerophilicity")
    expect_null(out$dropped)
    expect_false(dir.exists(file.path(tempdir(), "sources")))
})

test_that(".readZenodoZip reads the dropped imputations only when asked", {
    root <- tempfile("deposit")
    dir.create(root)
    tbl <- "# header line\nNCBI_ID,Attribute,Attribute_value\n1,Aerophilicity,aerobic\n"
    for (f in c("binary", "multistate", "numeric"))
        writeLines(tbl, file.path(root, paste0("bugphyzz_", f, ".csv")))
    gz <- gzfile(file.path(root, "bugphyzz_asr_dropped.csv.gz"), "w")
    writeLines(c("# header", "NCBI_ID,Attribute,Score", "2,aerophilicity,0.3"), gz)
    close(gz)
    zip <- tempfile(fileext = ".zip")
    withr::with_dir(root, utils::zip(zip, ".", flags = "-rq"))

    expect_null(bugphyzz:::.readZenodoZip(zip)$dropped)
    out <- bugphyzz:::.readZenodoZip(zip, dropped = TRUE)
    expect_equal(nrow(out$dropped), 1L)
    expect_identical(out$dropped$Score, "0.3")
})
