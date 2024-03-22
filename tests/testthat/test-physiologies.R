library(purrr)
pnames <- showPhys()
p <- physiologies()
test_that("physiologies works", {
  expect_true(all(map_lgl(p, is.data.frame)))
  expect_true(all(names(p) == pnames))
})
