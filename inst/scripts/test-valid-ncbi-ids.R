
phys <- physiologies()
phys[["fatty acid composition"]] <- fattyAcidComposition()

report <- purrr::map(phys, check_valid_ncbi_ids)
errors <- purrr::discard(report, is.null)

test_that("NCBI IDs exist.", {

  if (!length(errors)) {
    succeed(message = 'All NCBI IDs are valid and up to date.')
  }

  not_found <- purrr::map(errors, ~ dplyr::filter(.x, comment == 'not found'))
  expect_true(all(purrr::map_lgl(not_found, ~ nrow(.x) == 0)))

})

test_that("NCBI IDs are up to date.", {

  if (!length(errors)) {
    succeed(message = 'All NCBI IDs are valid and up to date.')
  }

  outdated <- purrr::map(errors, ~ dplyr::filter(.x, comment == 'outdated'))
  expect_true(all(purrr::map_lgl(outdated, ~ nrow(.x) == 0)))

})
