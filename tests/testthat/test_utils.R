test_that(".convertColor()", {
  testthat::skip_on_cran()
  local_edition(3)

  data <- matrix(c(65.29, 4.22, 15.62), ncol = 3)
  testthat::expect_type(.convertColor(data), "double")

})
