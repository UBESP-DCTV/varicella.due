test_that("compose_trvaltest_up_to_year works", {
  # setup
  db_test <- tibble::tribble(
    ~n_paz, ~anno,     ~class,
    1089L,  2004, "negative",
    1668L,  2004, "negative",
    1973L,  2004, "negative",
    1064L,  2004, "negative",
    3281L,  2004, "negative",
    3379L,  2004, "negative",
    3313L,  2004, "negative",
    1289L,  2004, "negative",
    1682L,  2004, "negative",
    1675L,  2004, "negative",
    1105L,  2004, "positive",
    1946L,  2004, "positive",
    1010L,  2004, "positive",
    1884L,  2004, "positive",
    1010L,  2004, "positive",
    1637L,  2004, "positive",
    1946L,  2004, "positive",
    3346L,  2004, "positive",
    1882L,  2004, "positive",
    1946L,  2004, "positive",
    1371L,  2005, "negative",
    3346L,  2005, "negative",
    3401L,  2005, "negative",
    5471L,  2005, "negative",
    3653L,  2005, "negative",
    1188L,  2005, "negative",
    1633L,  2005, "negative",
    4335L,  2005, "negative",
    3469L,  2005, "negative",
    65L,  2005, "negative",
    1765L,  2005, "positive",
    1841L,  2005, "positive",
    1818L,  2005, "positive",
    2389L,  2005, "positive",
    1469L,  2005, "positive",
    1931L,  2005, "positive",
    3549L,  2005, "positive",
    1671L,  2005, "positive",
    1301L,  2005, "positive",
    2781L,  2005, "positive",
    1959L,  2006, "negative",
    2344L,  2006, "negative",
    1657L,  2006, "negative",
    1236L,  2006, "negative",
    1920L,  2006, "negative",
    2005L,  2006, "negative",
    3810L,  2006, "negative",
    4392L,  2006, "negative",
    1658L,  2006, "negative",
    1445L,  2006, "negative",
    1439L,  2006, "positive",
    1679L,  2006, "positive",
    1850L,  2006, "positive",
    1932L,  2006, "positive",
    1943L,  2006, "positive",
    1278L,  2006, "positive",
    2507L,  2006, "positive",
    1833L,  2006, "positive",
    1738L,  2006, "positive",
    1844L,  2006, "positive",
    1275L,  2007, "negative",
    1696L,  2007, "negative",
    967L,  2007, "negative",
    3473L,  2007, "negative",
    1881L,  2007, "negative",
    3012L,  2007, "negative",
    4158L,  2007, "negative",
    2061L,  2007, "negative",
    4644L,  2007, "negative",
    843L,  2007, "negative",
    1280L,  2007, "positive",
    2111L,  2007, "positive",
    1267L,  2007, "positive",
    1672L,  2007, "positive",
    1905L,  2007, "positive",
    2289L,  2007, "positive",
    4448L,  2007, "positive",
    1427L,  2007, "positive",
    1804L,  2007, "positive",
    3623L,  2007, "positive",
    3907L,  2008, "negative",
    2319L,  2008, "negative",
    5162L,  2008, "negative",
    5785L,  2008, "negative",
    1459L,  2008, "negative",
    1947L,  2008, "negative",
    6928L,  2008, "negative",
    2219L,  2008, "negative",
    3483L,  2008, "negative",
    2043L,  2008, "negative",
    1329L,  2008, "positive",
    2072L,  2008, "positive",
    5216L,  2008, "positive",
    1329L,  2008, "positive",
    2223L,  2008, "positive",
    1653L,  2008, "positive",
    5192L,  2008, "positive",
    3466L,  2008, "positive",
    2483L,  2008, "positive",
    2235L,  2008, "positive"
  ) |>
    dplyr::mutate(
      class = .data[["class"]] |>
        factor(levels = c("negative", "positive")),
      id_medico = sample(10, 100, replace = TRUE),
      t_1 = sample(letters, 100, replace = TRUE),
      t_2 = sample(LETTERS, 100, replace = TRUE),
      fix = 1,
      date = sample(100, 100, replace = TRUE) + Sys.Date()
    ) |>
    dplyr::group_by(.data[["n_paz"]], .data[["anno"]]) |>
    dplyr::mutate(class = dplyr::if_else(
      "positive" %in% .data[["class"]], "positive", "negative"
    )) |>
    dplyr::ungroup()



  # evaluation
  db_2005 <- compose_trvaltest_up_to_year(db_test, 2005)

  # test

  ## 20 less removing 2008 (test up to 2007)
  expect_data_frame(db_2005, nrows = 80)

  ## one more for `set`
  expect_data_frame(db_2005, ncols = 9)

  expect_error(
    compose_trvaltest_up_to_year(db_test, 2003),
    "provide a value between the bounduaries"
  )
  expect_error(
    compose_trvaltest_up_to_year(db_test, 2014),
    "provide a value between the bounduaries"
  )
})
