test_that("merge_records works", {
  # setup
  db <- tibble::tribble(
        ~txt_1, ~txt_2, ~meta_txt, ~ meta_num,
     "faa tra",  "bar",     "tar",         12,
     "fee fir",     NA,     "ter",         NA,
            NA,  "bir",     "tir",         17,
         "foo",  "bor",        NA,         24,
         "fuu",  "bur",     "tur",         82
  ) |>
    dplyr::mutate(date = 5:1 + Sys.Date())

  meta_vars <- c("meta_txt", "meta_num", "date")

  # evaluate
  res <- merge_records(db, meta_vars)

  # test
  res |>
    expect_data_frame(
      ncols = 4,
      nrows = 1
    )
  expect_class(res$notes, "character")
  expect_equal(res$date[[1]], 1:5 + Sys.Date()) # ordered!

})
