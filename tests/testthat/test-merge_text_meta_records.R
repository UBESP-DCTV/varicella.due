test_that("merge_text_meta_records works", {
  # setup
  db <- tibble::tribble(
    ~med, ~n_paz, ~anno, ~class, ~set, ~t_1, ~t_2, ~meta_t, ~ meta_n, ~fix,
    1, 1, 2005, "positive", "train", "faa tra", "bar", "tar", 12,  1,
    2, 1, 2006, "negative", "validation", "fee fir", NA, "ter", NA, 1,

    1, 2, 2008, "negative", "train", NA, "bir", "tir", 17, 1,
    1, 3, 2006, "positive", "train", "foo", "bor", NA, 24, 1,

    # patient 4 has two id_medico qithin same year and set with
    # different class. "positive class" will predominate
    3, 4, 2004, "positive", "train", "fuu", "bur", "tur", 82, 1,
    4, 4, 2004, "negative", "train", "fuu", "bur", "tur", 82, 1
  ) |>
    dplyr::mutate(date = rev(1:6 + Sys.Date()))

  meta_vars <- c("med", "meta_t", "meta_n", "anno", "fix", "date")

  # evaluation
  res <- merge_text_meta_records(db, meta_vars)

  # test
  res |>
    expect_data_frame(
      ncols = 10,
      nrows = 5
    )
  expect_true(diff(res$date[[5]]) > 0) # ordered!
  expect_equal(res, dplyr::ungroup(res))
})
