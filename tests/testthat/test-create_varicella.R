test_that("create_varicella works", {
  # setup
  set.seed(1)

  db <- tibble::tibble(
    id_medico = sample(1:3, 100, replace = TRUE),
    n_paz = sample(1:100, 100, replace = TRUE),
    anno = sample(2004:2014, 100, replace = TRUE),
    date = as.character(sample(1:28, 100, replace = TRUE)) |>
      paste(as.character(sample(1:12, 100, replace = TRUE)), sep = "-") |>
      paste(anno, sep = "-") |>
      lubridate::dmy()
  )

  pop <- dplyr::distinct(db, id_medico, n_paz) |>
      dplyr::mutate(
        n = dplyr::n(),
        sesso = sample(c("female", "male"), n, replace = TRUE)
      ) |>
    dplyr::select(-n)

  gold <- tibble::tibble(
    n_paz = rep(1:4, each = 11),
    anno = rep(2004:2014, times = 4),
    class = factor(c("negative", "positive")) |>
      sample(size = 44, replace = TRUE, prob = c(0.7, 0.3))
  )

  # evaluate
  res <- create_varicella(db, pop, gold)

  # test
  expect_data_frame(res, nrows = 100, ncols = 6)

})
