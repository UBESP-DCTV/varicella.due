#' Compose dataset for training/test
#'
#' Starting from the dataset provided, it creates the
#' train/validation/test dataset consideraing training data up to
#' `year`. Test set wil be two years forward `year`.
#'
#' @param db (data frame) original dataset, must have a variable `anno`
#' @param year (int) integerish number form 2004 to 2013 (we have data
#'   up to 2014) so we cannot have more than 2013 data if we want to
#'   have data to predict
#'
#' @return
#' @export
#'
#' @examples
compose_trvaltest_up_to_year <- function(db, year) {
  if (year < 2004 || year > 2013) usethis::ui_stop(
    "{usethis::ui_field('year')} must be between 2004 and 2013.
    It is {usethis::ui_value(year)}.
    Please, provide a value between the bounduaries.
    "
  )

  train_val <- db |>
    dplyr::select(.data[["n_paz"]], .data[["anno"]], .data[["class"]]) |>
    dplyr::distinct() |>
    dplyr::filter(.data[["anno"]] <= year)

  validation <- train_val |>
    dplyr::group_by(.data[["anno"]], .data[["class"]]) |>
    dplyr::slice_sample(prop = 0.2) |>
    dplyr::ungroup() |>
    dplyr::mutate(set = "validation")

  train <- train_val |>
    dplyr::anti_join(validation, by = c("n_paz", "anno", "class")) |>
    dplyr::mutate(set = "train")

  test <- db |>
    dplyr::select(.data[["n_paz"]], .data[["anno"]], .data[["class"]]) |>
    dplyr::filter(.data[["anno"]] > year) |>
    dplyr::mutate(set = "test")

  train_val_test <- dplyr::bind_rows(train, validation, test)

  db |>
    dplyr::filter(.data[["anno"]] <= year + 2L) |>
    dplyr::left_join(train_val_test, by = c("n_paz", "anno", "class"))
}
