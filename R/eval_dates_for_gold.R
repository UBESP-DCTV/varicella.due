#' Create dated gold standard
#'
#' Create a version of the gold standard with the first date in which
#' a patient became positive to Zoster
#'
#' @param gold (df) the gold standard
#' @param varic_df the whole varicella dataset
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#'   # inside the development project
#'   library(targets)
#'
#'   gold <- tar_read(gold)
#'   df <- tar_read(varic_df)
#'
#'   eval_dates_for_gold(gold, varic_df)
#' }
#'
eval_dates_for_gold <- function(gold, varic_df) {
  pattern <- "([vV][aA][rR][iI][cC])|052"

  varic_df |>
    dplyr::filter(.data[["n_paz"]] %in% gold[["n_paz"]]) |>
    dplyr::filter(
      stringr::str_detect(.data[["motivo_accesso"]], pattern) |
        stringr::str_detect(.data[["motivo_accesso2"]], pattern) |
        stringr::str_detect(.data[["motivo_accesso3"]], pattern) |
        stringr::str_detect(.data[["diagnosi_ped"]], pattern) |
        stringr::str_detect(.data[["diario"]], pattern) |
        stringr::str_detect(.data[["diagnosi_ricovero"]], pattern) |
        stringr::str_detect(.data[["testo_soap"]], pattern) |
        stringr::str_detect(.data[["diagnosi_spec"]], pattern)
    ) |>
    dplyr::group_by(.data[["n_paz"]]) |>
    dplyr::filter(.data[["date"]] == min(.data[["date"]])) |>
    dplyr::ungroup() |>
    dplyr::distinct(
      .data[["n_paz"]], .data[["date"]], .keep_all = TRUE
    ) |>
    dplyr::inner_join(
      dplyr::distinct(gold, .data[["n_paz"]]),
      by = "n_paz"
    ) |>
    dplyr::select(.data[["n_paz"]], .data[["anno"]]) |>
    dplyr::mutate(class = "positive")
}
