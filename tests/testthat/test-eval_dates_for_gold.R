test_that("eval_dates_for_gold works", {
  skip_if(basename(here::here()) != "varicella.due")

  # setup
  global_gold <- tar_read_if_valid("globalGold")
  varic_df <- tar_read_if_valid("varic_df")

  # eval
  gold_with_dates <- eval_dates_for_gold(global_gold, varic_df)

  # test
  expect_set_equal(  # devo ricostruire il gold standard
    gold_with_dates[["n_paz"]],
    global_gold[["n_paz"]]
  )
  expect_equal(  # vogliamo record unici
    gold_with_dates,
    dplyr::distinct(gold_with_dates, n_paz, id_medico, .keep_all = TRUE)
  )
  expect_equal(c("id_medico", "n_paz", "anno", "class"), names(gold_with_dates))

  expect_equal(
    nrow(dplyr::distinct(global_gold, n_paz, id_medico)),
    nrow(dplyr::distinct(gold_with_dates, n_paz, id_medico))
  )

})
