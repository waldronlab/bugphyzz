## .importSources() on a tiny local manifest, no network.
test_that(".importSources reads a manifest and processes files like sheets", {
    d <- withr::local_tempdir()
    dir.create(file.path(d, "x"))
    writeLines(c("source\tphysiology\tattribute_type\tfile",
                 "Test\taerophilicity\tmultistate-intersection\tx/aero.tsv",
                 "Test\tgenome size\tnumeric\tx/gs.tsv"), file.path(d, "manifest.tsv"))
    writeLines(c("NCBI_ID\tGenome_ID\tAccession_ID\tTaxon_name\tAttribute\tAttribute_value\tAttribute_source\tFrequency\tNote",
                 "1386\t\t\tBacillus\taerobic\tTRUE\tBergeys_2026\tusually\tdoi",
                 "1386\t\t\tBacillus\tfacultatively anaerobic\tTRUE\tBergeys_2026\tsometimes\tdoi"), file.path(d, "x", "aero.tsv"))
    writeLines(c("NCBI_ID\tGenome_ID\tAccession_ID\tTaxon_name\tAttribute\tAttribute_value\tAttribute_source\tFrequency\tUnit\tNote",
                 "562\t\t\tEscherichia coli\tGenome size\t4600000\tMadin_2020\talways\tbase pair\t"), file.path(d, "x", "gs.tsv"))
    withr::local_options(bugphyzz.sourcesBaseUrl = paste0(d, "/"))
    out <- bugphyzz:::.importSources(c("aerophilicity", "genome size"), forceDownload = TRUE)
    expect_setequal(names(out), c("aerophilicity", "genome size"))
    a <- out$aerophilicity
    expect_equal(nrow(a), 2)
    expect_true(all(a$Attribute_value))
    expect_equal(unique(a$Attribute_type), "multistate-intersection")
    expect_equal(unique(a$Rank), "genus")
    g <- out[["genome size"]]
    expect_equal(g$Attribute_value_min, 4600000)
    expect_equal(unique(g$Attribute_type), "range")
    expect_equal(length(bugphyzz:::.importSources("habitat")), 0)
})
