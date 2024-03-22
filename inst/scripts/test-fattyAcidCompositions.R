test_that("fattyAcidComposition imports a data.frame", {
  fac <- bugphyzz:::.fattyAcidComposition()
  expect_s3_class(object = fac, class = "data.frame")
})
