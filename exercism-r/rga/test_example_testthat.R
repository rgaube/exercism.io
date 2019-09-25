library(testthat)
source("./example_testthat.R")

test_that(
  "first_of_last_month function v1",
  {
    expect_equal(first_of_last_month("2018-06-05"), "2018-05-01")
    expect_equal(first_of_last_month("2018-12-01"), "2018-11-01")
    expect_equal(first_of_last_month("2018-01-11"), "2017-12-01")
  }
)

test_that(
  "first_of_last_month function v2",
  {
    expect_equal(first_of_last_month_v2("2018-06-05"), "2018-05-01")
    expect_equal(first_of_last_month_v2("2018-12-01"), "2018-11-01")
    expect_equal(first_of_last_month_v2("2018-01-11"), "2017-12-01")
  }
)

test_that(
  "first_of_last_month function v3",
  {
    expect_equal(first_of_last_month_v3("2018-06-05"), "2018-05-01")
    expect_equal(first_of_last_month_v3("2018-12-01"), "2018-11-01")
    expect_equal(first_of_last_month_v3("2018-01-11"), "2017-12-01")
  }
)
