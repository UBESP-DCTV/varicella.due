test_that("meta_vars works", {
  # setup
  varicella <- tar_read_if_valid("varicella")

  # test
  expect_subset(meta_vars(), names(varicella))
  expect_subset(c("id_medico", "date"), meta_vars())
  expect_disjunct(c("class", "set", "n_paz", "anno", "notes"), meta_vars())
})
