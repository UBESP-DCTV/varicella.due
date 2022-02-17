test_that("create_varicella_mixdb works", {
  # setup
  test_db <- tibble::tribble(
    ~class, ~id_medico, ~n_paz, ~data, ~notes,      ~gender,
    0  ,   1, 1, "2010-03-22", "foo notes",     "male",
    0  ,   2, 2, "2010-03-23", "bar notes",     "female",
    1  ,   1, 3, "2010-04-22", "another notes", "male",
    1  ,   3, 1, "2011-03-22", "annotated foo", "female"
  ) |>
    dplyr::mutate(class = factor(.data[["class"]]))

  # evaluation
  # test_mixdb <- mixdb(test_db, meta_vars(id, gender))
  # test_meta <- attr(test_mixdb, "meta")
  # test_mixdb_dictionary <- attr(test_mixdb, "dictionary")
  #
})
