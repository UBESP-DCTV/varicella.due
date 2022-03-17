library(testthat)

format_metrics <- function(x, end_year, batch = 16) {
  x |>
    stringr::str_replace_all(" -", "\n-") |>
    stringr::str_replace_all("Epoch ([\\d/]+)", "\nEpoch \\1\n===========\n") |>
    stringr::str_replace_all(
      "(\\d+)/\\d+ \\[[.=>]+\\]",
      glue::glue("Batch size: {batch} records\nSteps: \\1")
    ) |>
    stringr::str_replace_all("\n- (\\d+s) (.+)", "\nTime: \\1 - \\2\n") |>
    stringr::str_replace_all(
      "- loss",
      glue::glue("\n\nTraining (2004-{end_year})\n--------------------\n\n- loss")
    ) |>
    stringr::str_replace_all(
      "- val_loss",
      glue::glue("\n\n\nTest ({end_year + 1}-{end_year + 2})\n----------------\n\n- val_loss")
    ) |>
    stringr::str_remove_all("\\.0+")
}

if (interactive()) {
  with_reporter(default_reporter(), {
    context("Formatting")
    test_that("format_metrics works", {


  # Setup -----------------------------------------------------------

      input <- "Epoch 2/15
  2110/2110 [==============================] - 3890s 2s/step - loss: 0.2380 - acc: 0.9271 - auroc: 0.9498 - auprc: 0.9399 - tp: 31246.0000 - tn: 31279.0000 - fp: 2473.0000 - fn: 2506.0000 - prec: 0.9267 - rec: 0.9258 - val_loss: 0.3015 - val_acc: 0.9390 - val_auroc: 0.9690 - val_auprc: 0.9639 - val_tp: 12624.0000 - val_tn: 13792.0000 - val_fp: 347.0000 - val_fn: 1515.0000 - val_prec: 0.9732 - val_rec: 0.8928"

      expected <- "
  Epoch 2/15
  ===========

  Batch size: 16 records
  Steps: 2110
  Time: 3890s - 2s/step


  Training (2004-2010)
  --------------------

  - loss: 0.2380
  - acc: 0.9271
  - auroc: 0.9498
  - auprc: 0.9399
  - tp: 31246
  - tn: 31279
  - fp: 2473
  - fn: 2506
  - prec: 0.9267
  - rec: 0.9258


  Test (2011-2012)
  ----------------

  - val_loss: 0.3015
  - val_acc: 0.9390
  - val_auroc: 0.9690
  - val_auprc: 0.9639
  - val_tp: 12624
  - val_tn: 13792
  - val_fp: 347
  - val_fn: 1515
  - val_prec: 0.9732
  - val_rec: 0.8928"


  # Evaluation ------------------------------------------------------

     res <- format_metrics(input, 2010)
     cat(res)


  # Tests -----------------------------------------------------------

      expect_equal(res, expected)
    })
  })
}
