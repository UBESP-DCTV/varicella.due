test_that("multiplication works", {
  # setup
  first <- 2
  second <- 2

  # execution
  prod <- first * second

  # tests
  expect_equal(prod, 4)
})
