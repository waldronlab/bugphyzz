physiologies_list <- c(
  "animal pathogen",
  "antimicrobial resistance",
  "antimicrobial sensitivity",
  "biofilm forming",
  "butyrate producing",
  "acetate producing",
  "lactate producing",
  "arrangement",
  "shape",
  "COGEM pathogenicity rating",
  "mutation rate per site per generation",
  "mutation rates per site per year",
  "extreme environment",
  "gram stain",
  "growth medium",
  "growth temperature",
  "habitat",
  "optimal ph",
  "aerophilicity",
  "plant pathogenicity",
  "width",
  "spore shape",
  "isolation site",
  "disease association",
  "hydrogen gas producing",
  "length",
  "health associated"
)

lodf <- bugphyzz::physiologies()

for (physiology in physiologies_list) {
  test_that(paste(physiology, "has required columns."), {
    expect_message(
      bugphyzz:::.checkRequiredColumnsDF(lodf[[physiology]]),
      paste(">>> All columns are present and in the right order.")
    )
  })

  test_that(paste(physiology, "has valid data."), {
    expect_message(
      bugphyzz:::.checkColumnValuesDF(lodf[[physiology]]),
      paste("All values are valid in the current dataset.")
    )
  })
}

test_that("physiologies displays an error for an unexpected keyword.", {
  expect_error(bugphyzz::physiologies("something that doesn't exist"))
})

test_that("curationLinks displays all links", {
  cl <- bugphyzz:::curationLinks()
  expect_identical(cl$physiology, physiologies_list)
  expect_true("link" %in% colnames(cl))
  expect_equal(c(rep(TRUE, length(cl$link))),
               grepl("https://docs.google.com/spreadsheets/", cl$link))
})

test_that("curationLinks displays animal pathogen and gram stain.", {
  keywords <- c("animal pathogen", "gram stain")
  cl <- bugphyzz:::curationLinks(keywords)
  expect_equal(cl$physiology, keywords)
})

test_that("physiologiesList displays all physiologies.", {
  expect_identical(bugphyzz::physiologiesList(), physiologies_list)
})
