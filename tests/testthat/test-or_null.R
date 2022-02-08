test_that("%||% works", {
  # setup
  null <- NULL
  one <- 1

  # evaluate
  with_null <- null %||% 2
  without_null <- one %||% 2

  # test
  expect_equal(with_null, 2)
  expect_equal(without_null, 1)
})
