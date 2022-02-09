test_that("eval_dates_for_gold works", {
  # setup
  gold <- tar_read_if_valid("gold")
  varic_df <- tar_read_if_valid("varic_df")

  # eval
  gold_with_dates <- eval_dates_for_gold(gold, varic_df)

  # test
  expect_set_equal( # devo ricostruire il gold standard
    gold_with_dates[["nPaz"]],
    gold[["nPaz"]]
  )
  expect_equal( # vogliamo record unici
    gold_with_dates,
    dplyr::distinct(gold_with_dates, nPaz, Id_Medico, .keep_all = TRUE)
  )
  expect_subset("Data", names(gold_with_dates))

  expect_equal(
    nrow(dplyr::distinct(gold, nPaz, Id_Medico )),
    nrow(dplyr::distinct(gold_with_dates, nPaz, Id_Medico))
  )

})
