test_that("get_data_path works", {
  # setup
  data_path <- get_data_path()

  # evaluate
  popolazione <- file.path(data_path, "popolazione.rds")
  positive_gold <- file.path(data_path, "positive_gold.rds")
  varic_df <- file.path(data_path, "varic_df.rds")

  # test
  expect_file_exists(popolazione)
  expect_file_exists(positive_gold)
  expect_file_exists(varic_df)
})

test_that("environmental variable is set", {
  # setup
  withr::local_envvar(list(DATA_PATH = NULL))

  # test
  expect_error(get_data_path(), "must be set")
})
