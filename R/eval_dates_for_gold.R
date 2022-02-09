#' Create dated gold standard
#'
#' Create a version of the gold stadnard with the first date in which
#' a patient became positive to Zoster
#'
#' @param gold (df) the gold standard
#' @param varic_df the whole varicalla dataset
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
    dplyr::distinct() |>
    dplyr::filter(nPaz %in% gold[["nPaz"]]) |>
    dplyr::filter(
      stringr::str_detect(.data[["MotivoAccesso"]], pattern) |
        stringr::str_detect(.data[["MotivoAccesso2"]], pattern) |
        stringr::str_detect(.data[["MotivoAccesso3"]], pattern) |
        stringr::str_detect(.data[["Diagnosi_Ped"]], pattern) |
        stringr::str_detect(.data[["Diario"]], pattern) |
        stringr::str_detect(.data[["Diagnosi_Ricovero"]], pattern) |
        stringr::str_detect(.data[["Testo_Soap"]], pattern) |
        stringr::str_detect(.data[["Diagnosi_Spec"]], pattern)
    ) |>
    dplyr::group_by(nPaz, Id_Medico) |>
    dplyr::filter(.data[["Data"]] == min(.data[["Data"]])) |>
    dplyr::ungroup() |>
    dplyr::distinct(Id_Medico, nPaz, Data, .keep_all = TRUE) |>
    dplyr::inner_join(dplyr::distinct(gold, Id_Medico, nPaz))
}
