test_that("plot_colProfile()", {
  testthat::skip_on_cran()
  local_edition(3)

  ##break function
  testthat::expect_error(plot_colProfile(data = "test"), "*.data must be a data\\.frame.*")
  testthat::expect_error(plot_colProfile(data = matrix(1:10, ncol = 1)), "*.data must be a data\\.frame.*")

  ##run the function
  data(LESLIE_profile, envir = environment())
    ## standard plot output
    testthat::expect_silent(plot_colProfile(LESLIE_profile, cycles = 3))

    ## standard plot output landscape mode
    testthat::expect_silent(plot_colProfile(LESLIE_profile, cycles = 3, orientation = "landscape"))

    ## show cycle 0, 1 and 3
    testthat::expect_silent(plot_colProfile(LESLIE_profile, cycles = c(1,3)))

    ## show only cycle 3 and not the non-enhanced colour profile
    testthat::expect_silent(plot_colProfile(LESLIE_profile, cycles = c(-1,3)))

    ## test the object output
    testthat::expect_type(plot_colProfile(LESLIE_profile, plot = FALSE), "list")

})
