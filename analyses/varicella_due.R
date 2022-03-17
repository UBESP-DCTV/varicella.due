library(tidyverse)
library(here)
library(future)
plan(multisession)

# Functions -------------------------------------------------------


start_ids <- function(txt) {
  str_which(txt, "start target")
}

end_ids <- function(txt) {
  str_which(txt, "built target")
}

add_epoch <- function(txt) {
  which_epochs <- txt |>
    str_detect("^Epoch") |>
    cumsum()

  paste("epoch", which_epochs, txt)
}


split_trainings <- function(txt) {
  starts <- start_ids(txt)
  ends <- end_ids(txt)
  names <- txt[starts] |>
    stringr::str_extract("\\d{4}$")

  purrr::map2(starts, ends, ~{
      txt[.x:.y] |>
        add_epoch()
    }) |>
    purrr::set_names(names)
}



subset_progress <- function(txt) {
  txt |>
    stringr::str_subset("\\[[=.>]+\\]") |>
    stringr::str_remove("\b+")
}

parse_metrics <- function(txt) {
  if (length(txt) > 1) {
    return(
      purrr::map_dfr(txt, parse_metrics)
    )
  }

  checkmate::check_string(txt)

  type <- if (stringr::str_detect(txt, "val_")) {
      c("train", "test")
    } else {
      "train"
    }

  epoch <- txt |>
    stringr::str_extract("(?<=epoch )\\d+") |>
    unlist() |>
    as.numeric()

  batch <- txt |>
    stringr::str_extract_all("\\d+(?=/)") |>
    unlist() |>
    as.numeric()


  batchs <- txt |>
    stringr::str_extract_all("(?<=/)\\d+") |>
    unlist() |>
    as.numeric()


  eta_aux <- txt |>
    stringr::str_extract_all("(?<=ETA: )\\d+") |>
    unlist() |>
    as.numeric()
  eta <- if (!length(eta_aux)) c(0, NA) else eta_aux



  time_aux <- txt |>
    stringr::str_extract_all("(?<=- )\\d+(?=s)") |>
    unlist() |>
    as.numeric()
  time <- if (length(time_aux)) c(NA, time_aux) else NA_real_

  freq_aux <-  txt |>
    stringr::str_extract_all("\\d+(?=s/step)") |>
    unlist() |>
    as.numeric()
  freq <- if (length(freq_aux)) {
      c(NA, round(1/freq_aux, 2))
    } else {
      NA_real_
    }

  # the following automatically captur loss and val_loss
  loss <- txt |>
    stringr::str_extract_all("(?<=loss: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()

  acc <- txt |>
    stringr::str_extract_all("(?<=acc: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  auroc <- txt |>
    stringr::str_extract_all("(?<=auroc: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  auprc <- txt |>
    stringr::str_extract_all("(?<=auprc: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  # as.numeric automatically remove/ignores trailing zeros
  tp <- txt |>
    stringr::str_extract_all("(?<=tp: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  tn <- txt |>
    stringr::str_extract_all("(?<=tn: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  fp <- txt |>
    stringr::str_extract_all("(?<=fp: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  fn <- txt |>
    stringr::str_extract_all("(?<=fn: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  prec <- txt |>
    stringr::str_extract_all("(?<=prec: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()


  # must be manage difference from "prec"
  rec <- txt |>
    stringr::str_extract_all("(?<=[ _]rec: )[\\d\\.]+") |>
    unlist() |>
    as.numeric()



  tibble::tibble(
    type = type,
    epoch = epoch,
    batch = batch,
    batchs = batchs,
    eta = eta,
    time = time,
    freq = freq,
    loss = loss,
    acc = acc,
    auroc = auroc,
    auprc = auprc,
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    prec = prec,
    rec = rec
  )
}

read_varicella_log <- function(path, years = NULL, testonly = FALSE) {
  splitted <- read_lines(path, num_threads = 1L) |>
    stringr::str_split("\r") |>
    unlist() |>
    split_trainings()

  # years is set for testing purpose only (reducing samples)
  years <- if (is.null(years)) length(splitted) else years
  subsetted <- purrr::map(splitted[seq_len(years)], subset_progress)

  res <- subsetted |>
    furrr::future_map_dfr(~{
      records <- if (testonly) {
        sample.int(length(.x), 100)
      } else {
        seq_along(.x)
      }

      parse_metrics(.x[records])
      },
      .id = "year",
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
    )

  invisible(res)
}




# Analyses/Executions ---------------------------------------------

vari_log_path <- here("~/ubesp/20220307T212321-keras_run.log")
out_path <- here("~/ubesp/varicella_due.rds")

if (FALSE) {  # to execute once only
  vari_log <- read_varicella_log(vari_log_path)
  readr::write_rds(vari_log, out_path)
}

if (fs::file_exists(out_path)) {
  vari_log <- readr::read_rds(out_path)
}

vari_log |>
  mutate(
    year = paste("2004", .data[["year"]], sep = " - ")
  ) |>
  ggplot(aes(x = epoch, y = auroc, colour = type)) +
  geom_smooth() +
  facet_grid(~year, scales = "free")


vari_log |>
  filter(type == "train") |>
  mutate(
    year = paste("2004", .data[["year"]], sep = " - ")
  ) |>
  ggplot(aes(x = batch, y = auroc, colour = epoch)) +
  geom_point() +
  facet_grid(~year, scales = "free")





  # tests -----------------------------------------------------------



# tests -----------------------------------------------------------

library(testthat)
library(checkmate)

with_reporter(check_reporter(), {
  context("")
  test_that("read_varicella_log works", {
    # skip("too earlyforthis test")
    # setup
    log <- here("~/ubesp/20220307T212321-keras_run.log")


    # eval
    res <- expect_invisible(
      read_varicella_log(log, years = 1, testonly = TRUE)
    )

    # tests
    expect_tibble(res, ncols = 18) # metrics + year

  })

  test_that("start_id and end_id work", {
    # setup
    str <- c(
    "✔ skip target adjustesMaxWords_2009",
    "• start target testBidirectionalDeepGru_2010",
    "Loaded Tensorflow version 2.8.0",
    "• built target testBidirectionalDeepGru_2010",
    "• start target testBidirectionalDeepGru_2011",
    "Model: \"model_1\"",
    "• built target testBidirectionalDeepGru_2011"
    )

    # eval
    starts <- start_ids(str)
    ends <- end_ids(str)
    separated <- split_trainings(str)

    # tests
    expect_equal(starts, c(2, 5))
    expect_equal(ends, c(4, 7))
    expect_list(separated)

    expected_sep <- list(
      `2010` = c(
        "epoch 0 • start target testBidirectionalDeepGru_2010",
        "epoch 0 Loaded Tensorflow version 2.8.0",
        "epoch 0 • built target testBidirectionalDeepGru_2010"
      ),
      `2011` = c(
        "epoch 0 • start target testBidirectionalDeepGru_2011",
        "epoch 0 Model: \"model_1\"",
        "epoch 0 • built target testBidirectionalDeepGru_2011"
      )
    )
    separated |> expect_equal(expected_sep)
  })


  test_that("add_epoch works", {
    # setup
    str <- c(
      "foo",
      "Epoch 1/15",
      "bar",
      "Epoch 2/15"
    )

    # evaluation
    result <- add_epoch(str)

    # tests
    expected <- c(
      "epoch 0 foo",
      "epoch 1 Epoch 1/15",
      "epoch 1 bar",
      "epoch 2 Epoch 2/15"

    )
    result |> expect_equal(expected)
  })

  test_that("subset_progress works",{
    # setup
    str <- c(
      "• start target testBidirectionalDeepGru_2010",
      "1686/1687 [============================>.] - ETA: 1s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23794.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919",
"1687/1687 [==============================] - ETA: 0s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919",
"1687/1687 [==============================] - 3132s 2s/step - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919 - val_loss: 0.2415 - val_acc: 0.9690 - val_auroc: 0.6524 - val_auprc: 0.2787 - val_tp: 116.0000 - val_tn: 13303.0000 - val_fp: 129.0000 - val_fn: 300.0000 - val_prec: 0.4735 - val_rec: 0.2788",
"• built target testBidirectionalDeepGru_2009"
    )

    # execute
    result <- subset_progress(str)

    # tests
    expected <- c(
      "1686/1687 [============================>.] - ETA: 1s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23794.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919",
      "1687/1687 [==============================] - ETA: 0s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919",
      "1687/1687 [==============================] - 3132s 2s/step - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919 - val_loss: 0.2415 - val_acc: 0.9690 - val_auroc: 0.6524 - val_auprc: 0.2787 - val_tp: 116.0000 - val_tn: 13303.0000 - val_fp: 129.0000 - val_fn: 300.0000 - val_prec: 0.4735 - val_rec: 0.2788"
    )
    result |>
      expect_equal(expected)
  })


  test_that("parse_metrics works", {
    # setup
    str_base <-
      "epoch 2 1686/1687 [============================>.] - ETA: 1s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23794.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919"

    str_last <-
      "epoch 2 1687/1687 [==============================] - ETA: 0s - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919"

    str_final <-
      "epoch 2 1687/1687 [==============================] - 3132s 2s/step - loss: 0.1192 - acc: 0.9457 - auroc: 0.9803 - auprc: 0.6571 - tp: 1717.0000 - tn: 23796.0000 - fp: 1451.0000 - fn: 14.0000 - prec: 0.5420 - rec: 0.9919 - val_loss: 0.2415 - val_acc: 0.9690 - val_auroc: 0.6524 - val_auprc: 0.2787 - val_tp: 116.0000 - val_tn: 13303.0000 - val_fp: 129.0000 - val_fn: 300.0000 - val_prec: 0.4735 - val_rec: 0.2788"


    # evaluation
    res_base <- parse_metrics(str_base)
    res_last <- parse_metrics(str_last)
    res_final <- parse_metrics(str_final)

    res_combined <- parse_metrics(c(str_base,str_last, str_final))



    # tests
    expected_base <- tibble::tribble(
      ~type, ~epoch, ~batch, ~batchs, ~eta, ~time, ~freq, ~loss, ~acc, ~auroc, ~auprc, ~tp, ~tn, ~fp, ~fn, ~prec, ~rec,
      "train", 2, 1686, 1687, 1, NA_real_, NA_real_, 0.1192, 0.9457, 0.9803, 0.6571, 1717, 23794, 1451, 14, 0.542, 0.9919
    )
    expected_last <- tibble::tribble(
      ~type, ~epoch, ~batch, ~batchs, ~eta, ~time, ~freq, ~loss, ~acc, ~auroc, ~auprc, ~tp, ~tn, ~fp, ~fn, ~prec, ~rec,
      "train", 2, 1687, 1687, 0, NA_real_, NA_real_, 0.1192, 0.9457, 0.9803, 0.6571, 1717, 23796, 1451, 14, 0.542, 0.9919
    )
    expected_final <- tibble::tribble(
      ~type, ~epoch, ~batch, ~batchs, ~eta, ~time, ~freq, ~loss, ~acc, ~auroc, ~auprc, ~tp, ~tn, ~fp, ~fn, ~prec, ~rec,
      "train", 2, 1687, 1687, 0, NA_real_, NA_real_, 0.1192, 0.9457, 0.9803, 0.6571, 1717, 23796, 1451, 14, 0.542, 0.9919,
      "test", 2, 1687, 1687, NA_real_, 3132, 0.5, 0.2415, 0.9690, 0.6524, 0.2787, 116, 13303, 129, 300, 0.4735, 0.2788
    )

    res_base |> expect_equal(expected_base)
    res_last |> expect_equal(expected_last)
    res_final |> expect_equal(expected_final)

    res_combined |>
      expect_equal(dplyr::bind_rows(
        expected_base,expected_last, expected_final
      ))


  })


})


