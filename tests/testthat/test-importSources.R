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
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
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

test_that("physiologies() merges sources and sources = FALSE skips them", {
    testthat::skip_if_offline()
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
    d <- withr::local_tempdir()
    writeLines(c("source\tphysiology\tattribute_type\tfile", "Test\tsphingolipid producing\tbinary\tsp.tsv"), file.path(d, "manifest.tsv"))
    writeLines(c("NCBI_ID\tGenome_ID\tAccession_ID\tTaxon_name\tAttribute\tAttribute_value\tAttribute_source\tFrequency\tNote",
                 "562\t\t\tEscherichia coli\tsphingolipid producing\tTRUE\tBergeys_2026\talways\tdoi"), file.path(d, "sp.tsv"))
    withr::local_options(bugphyzz.sourcesBaseUrl = paste0(d, "/"))
    with_src <- suppressMessages(physiologies("sphingolipid producing"))[[1]]
    without <- suppressMessages(physiologies("sphingolipid producing", sources = FALSE))[[1]]
    expect_equal(nrow(with_src), nrow(without) + 1)
    expect_true("Bergeys_2026" %in% with_src$Attribute_source)
    expect_false("Bergeys_2026" %in% without$Attribute_source)
    expect_equal(with_src$Evidence[with_src$Attribute_source == "Bergeys_2026"], "exp")
})

test_that("physiologies() warns and continues when sources are unreachable", {
    testthat::skip_if_offline()
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
    withr::local_options(bugphyzz.sourcesBaseUrl = paste0(withr::local_tempdir(), "/nowhere/"))
    expect_warning(suppressMessages(physiologies("sphingolipid producing")), "Programmatic sources")
})

test_that(".modifyRange keeps both ends of a range", {
    df <- data.frame(Attribute_value = c("66.0-68.0", "65.29", "<5", ">10"), stringsAsFactors = FALSE)
    r <- bugphyzz:::.modifyRange(df)
    expect_equal(r$Attribute_value_min, c(66, 65.29, -Inf, 10))
    expect_equal(r$Attribute_value_max, c(68, 65.29, 5, Inf))
})

test_that(".getBacDive reads the gzipped strain table from the sources base URL", {
    withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
    d <- withr::local_tempdir(); dir.create(file.path(d, "bacdive"))
    con <- gzfile(file.path(d, "bacdive", "bacdive.csv.gz"), "w")
    writeLines(c("BacDive_ID,Taxon_name,NCBI_ID,Rank,Parent_name,Parent_NCBI_ID,Parent_rank,Seq16S_NCBI_ID,Genome_ID,Type_strain,Gram_stain,Cell_shape,Motility,Oxygen_tolerance,Halophily",
                 "1,Acetobacter aceti B 4114,,strain,Acetobacter aceti,435,species,,,no,negative,rod-shaped,no,aerobe;facultative anaerobe,NaCl positive growth 0-5 %;NaCl no growth 8 %"), con); close(con)
    withr::local_options(bugphyzz.sourcesBaseUrl = paste0(d, "/"))
    bd <- bugphyzz:::.getBacDive(forceDownload = TRUE)
    expect_true(all(c("gram stain", "shape", "motility", "aerophilicity") %in% bd$Attribute))
    expect_equal(bd$Parent_NCBI_ID[1], "435")
    ## multi-valued cells become one row per value; halophily keeps its ';' for its own parser
    expect_equal(sort(bd$Attribute_value[bd$Attribute == "aerophilicity"]), c("aerobe", "facultative anaerobe"))
    expect_equal(bd$Attribute_value[bd$Attribute == "halophily"], "NaCl positive growth 0-5 %;NaCl no growth 8 %")
    ## second call hits the cache
    bd2 <- bugphyzz:::.getBacDive(forceDownload = FALSE)
    expect_equal(nrow(bd2), nrow(bd))
})
