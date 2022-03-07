test_that("historical_information(information_type = 'BindHistoryInfo') returns dataframe", {
  expect_s3_class(
    historical_information(
      information_type = "BindHistoryInfo",
      fund_type = "YAT",
      start_date = as.Date('2021-10-01'),
      end_date = as.Date('2021-10-10')),
    "data.frame"
  )
})


test_that("historical_information(information_type = 'BindHistoryAllocation') returns dataframe", {
  expect_s3_class(
    historical_information(
      information_type = "BindHistoryAllocation",
      fund_type = "YAT",
      start_date = as.Date('2021-10-01'),
      end_date = as.Date('2021-10-10')),
    "data.frame"
  )
})
