#' @import testthat
library(testthat)
# Checking some value for
test_that("check_fars_summarize_years_vals",{
     expect_that(fars_summarize_years(2013)[[12,2]], equals(2457))
     expect_that(fars_summarize_years(2014)[[1,2]], equals(2168))
     expect_that(fars_summarize_years(2015)[[3,2]], equals(2385))})

test_that("check_fars_summarize_years_class", {
     expect_that( fars_summarize_years(c(2013,2014)), is_a("data.frame") )})
