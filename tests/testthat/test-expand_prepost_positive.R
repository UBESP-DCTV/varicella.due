test_that("expand_prepost_positive works", {
  # setup
  db_gold <- tibble::tribble(
    ~n_paz, ~anno, ~class,
    1, 2005L, "positive",
    2, 2008L, "positive",
    3, 2007L, "positive"
  )

  pop <- data.frame(
    id_medico = c(1, 2, 1, 3, 3),
    n_paz = c(1, 1, 2, 3, 4),
    foo = "bar"
  )

  expected <- tibble::tribble(
    ~n_paz, ~anno,            ~class,
    # 1 positive in 2005
    1, 2004L,        "negative",
    1, 2005L,        "positive",
    1, 2006L,        "negative",
    1, 2007L,        "negative",
    1, 2008L,        "negative",
    1, 2009L,        "negative",
    1, 2010L,        "negative",
    1, 2011L,        "negative",
    1, 2012L,        "negative",
    1, 2013L,        "negative",
    1, 2014L,        "negative",

    # 2 positive in 2008
    2, 2004L,        "negative",
    2, 2005L,        "negative",
    2, 2006L,        "negative",
    2, 2007L,        "negative",
    2, 2008L,        "positive",
    2, 2009L,        "negative",
    2, 2010L,        "negative",
    2, 2011L,        "negative",
    2, 2012L,        "negative",
    2, 2013L,        "negative",
    2, 2014L,        "negative",

    # positive in 2007
    3, 2004L,        "negative",
    3, 2005L,        "negative",
    3, 2006L,        "negative",
    3, 2007L,        "positive",
    3, 2008L,        "negative",
    3, 2009L,        "negative",
    3, 2010L,        "negative",
    3, 2011L,        "negative",
    3, 2012L,        "negative",
    3, 2013L,        "negative",
    3, 2014L,        "negative",

    # 4 all negative
    4, 2004L,        "negative",
    4, 2005L,        "negative",
    4, 2006L,        "negative",
    4, 2007L,        "negative",
    4, 2008L,        "negative",
    4, 2009L,        "negative",
    4, 2010L,        "negative",
    4, 2011L,        "negative",
    4, 2012L,        "negative",
    4, 2013L,        "negative",
    4, 2014L,        "negative"
  ) |>
    dplyr::mutate(
      class = class |>
        factor(levels = c("negative", "positive"))
    )

  # eval
  res <- expand_prepost_positive(db_gold, pop)

  # test
  expect_equal(res, expected)
})
