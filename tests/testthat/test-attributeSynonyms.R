syn <- bugphyzz:::.attributeSynonyms()

test_that("synonym table targets are valid attributes for their group", {
    expect_identical(colnames(syn), c("attribute_group", "source_term", "attribute"))
    expect_false(anyDuplicated(syn) > 0)
    mapped <- syn[nzchar(syn$attribute), ]
    ok <- mapply(function(g, a) a %in% bugphyzz:::.validAttributes(g),
                 mapped$attribute_group, mapped$attribute)
    expect_true(all(ok), info = paste(mapped$attribute[!ok], collapse = ", "))
})

test_that(".applySynonyms maps, splits, drops and leaves other terms alone", {
    df <- data.frame(
        NCBI_ID = c("1", "2", "3", "4", "5"),
        Attribute = c("rod-shaped", "coccus", "other", "vibriod cell", "rod-shaped"),
        Attribute_value = TRUE, Attribute_group = "shape"
    )
    out <- bugphyzz:::.applySynonyms(df)
    expect_identical(out$Attribute, c("bacillus", "coccus", "vibrio", "bacillus"))
    expect_identical(out$NCBI_ID, c("1", "2", "4", "5"))
    expect_identical(colnames(out), colnames(df))

    da <- data.frame(
        NCBI_ID = "1", Attribute = "meningitis septicemia",
        Attribute_value = TRUE, Attribute_group = "disease association"
    )
    expect_setequal(
        bugphyzz:::.applySynonyms(da)$Attribute, c("meningitis", "septicemia")
    )

    ## FALSE rows keep their source term (some mappings are lossy)
    neg <- data.frame(
        NCBI_ID = "1", Attribute = c("aerotolerant", "aerotolerant"),
        Attribute_value = c(TRUE, FALSE), Attribute_group = "aerophilicity"
    )
    expect_identical(
        bugphyzz:::.applySynonyms(neg)$Attribute, c("anaerobic", "aerotolerant")
    )
    ## terms map only within their own group
    other <- data.frame(
        NCBI_ID = "1", Attribute = "other", Attribute_value = TRUE,
        Attribute_group = "gram stain"
    )
    expect_identical(bugphyzz:::.applySynonyms(other), other)

    ## a group with no synonyms and a table without Attribute pass through
    gs <- data.frame(NCBI_ID = "1", Attribute = "x", Attribute_group = "length")
    expect_identical(bugphyzz:::.applySynonyms(gs), gs)
    rng <- data.frame(NCBI_ID = "1", Attribute_group = "shape")
    expect_identical(bugphyzz:::.applySynonyms(rng), rng)
})

test_that("Wikipedia_2026 and its class physiologies are in the package tables (#296)", {
    src <- utils::read.delim(
        system.file("extdata", "attribute_sources.tsv", package = "bugphyzz"),
        quote = ""
    )
    w <- src[src$Attribute_source == "Wikipedia_2026", ]
    expect_identical(nrow(w), 1L)
    expect_identical(w$Confidence_in_curation, "medium")
    expect_identical(w$Evidence, "nas")
    classes <- c("halophily class", "temperature class", "ph class")
    expect_true(all(classes %in% showPhys()))
    expect_setequal(
        bugphyzz:::.validAttributes("halophily class"),
        c("halophilic", "halotolerant", "non-halophilic")
    )
    expect_true(all(
        c("psychrotolerant", "thermophile") %in%
            bugphyzz:::.validAttributes("temperature class")
    ))
    expect_true(all(
        c("acidophile", "neutrophile") %in% bugphyzz:::.validAttributes("ph class")
    ))
    expect_true("skin" %in% bugphyzz:::.validAttributes("habitat"))
})
