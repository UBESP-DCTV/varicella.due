test_that("expand_prepost_positive works", {
  # setup
  db <- tibble::tribble(
    ~ Id_Medico, ~nPaz, ~Data, ~other, ~anno,
         1, 1, lubridate::ymd("2005-12-31"), "foo", 2005L,
         2, 1, lubridate::ymd("2005-12-31"), "foo", 2005L,
         1, 2, lubridate::ymd("2008-12-31"), "foo", 2008L,
         3, 3, lubridate::ymd("2007-12-31"), "foo", 2007L
  )

  expected <- tibble::tribble(
    ~ Id_Medico, ~nPaz, ~Data, ~other, ~anno, ~is_was_positive,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2004L, FALSE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2005L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2006L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2007L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2008L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2009L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2010L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2011L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2012L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2013L, TRUE,
    1, 1, lubridate::ymd("2005-12-31"), "foo", 2014L, TRUE,

    2, 1, lubridate::ymd("2005-12-31"), "foo", 2004L, FALSE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2005L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2006L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2007L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2008L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2009L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2010L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2011L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2012L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2013L, TRUE,
    2, 1, lubridate::ymd("2005-12-31"), "foo", 2014L, TRUE,

    1, 2, lubridate::ymd("2008-12-31"), "foo", 2004L, FALSE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2005L, FALSE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2006L, FALSE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2007L, FALSE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2008L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2009L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2010L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2011L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2012L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2013L, TRUE,
    1, 2, lubridate::ymd("2008-12-31"), "foo", 2014L, TRUE,

    3, 3, lubridate::ymd("2007-12-31"), "foo", 2004L, FALSE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2005L, FALSE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2006L, FALSE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2007L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2008L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2009L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2010L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2011L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2012L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2013L, TRUE,
    3, 3, lubridate::ymd("2007-12-31"), "foo", 2014L, TRUE

  )

  # eval
  res <- expand_prepost_positive(db)

  # test
  expect_equal(res, expected)
})
