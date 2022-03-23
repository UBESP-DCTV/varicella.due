test_that("read_varic works", {
  skip_if(basename(here::here()) != "varicella.due")

  # setup
  raw_popolazione <- tar_read_if_valid("rawPopolazione")
  raw_gold <- tar_read_if_valid("rawGold")
  raw_df <- tar_read_if_valid("rawDf")

  # evaluate
  popolazione <- read_varic(raw_popolazione, "popolazione")
  gold <- read_varic(raw_gold, "positive_gold")
  varic_df <- read_varic(raw_df, "varic_df")

  # test
  expect_data_frame(popolazione)
  expect_data_frame(gold)
  expect_data_frame(varic_df)

  expect_error(read_varic(raw_popolazione, "foo"))
})

test_that("read_varic returns cleaned names", {
  skip_if(basename(here::here()) != "varicella.due")

  # setup
  raw_popolazione <- tar_read_if_valid("rawPopolazione")
  raw_gold <- tar_read_if_valid("rawGold")
  raw_df <- tar_read_if_valid("rawDf")
  popolazione <- read_varic(raw_popolazione, "popolazione")
  gold <- read_varic(raw_gold, "positive_gold")
  varic_df <- read_varic(raw_df, "varic_df")

  # evaluate
  popolazione_cleaned <- janitor::clean_names(popolazione)
  gold_cleaned <- janitor::clean_names(gold)
  varic_df_cleaned <- janitor::clean_names(varic_df)

  # test
  expect_named(popolazione, names(popolazione_cleaned))
  expect_named(gold, names(gold))
  expect_named(varic_df, names(varic_df))
})
