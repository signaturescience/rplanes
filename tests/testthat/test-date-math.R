test_that("Basic date math works", {
})

test_that("Error handling works", {
  expect_error(valid_dates(seed_date = 99999, signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = 99999, resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-08"), resolution = "foo"))
  expect_warning(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-09"), resolution = "weekly", warn_incomplete = TRUE))
})
