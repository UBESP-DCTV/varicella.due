#' Expand previous and post positive status
#'
#' Given a dataset with columns `Id_Medico`, `nPaz`, and `Data`
#' (the latter referred to the year of Varicella infection), it adds
#' rows for each combination of `Id_Medico` and `nPaz` reporting the
#' status of "they was infected" for all the years from 2004 to 2014
#' in a column named `is_was_positive`
#'
#' @param db (data frame) positive database with `Id_Medico`, `nPaz`,
#' and `Data` as the minimum set of columns. Column `Data` reporting the
#' year of the Varicella infection for the corresponding patient.
#'
#' @return the original [tibble][tibble::tibble-package] with the
#' `is_was_positive` column added as last one.
#' @export
#'
#' @examples
#' db <- tibble::tribble(
#'   ~ Id_Medico, ~nPaz, ~Data, ~other, ~anno,
#'   1, 1, lubridate::ymd("2005-12-31"), "foo", 2005L,
#'   2, 1, lubridate::ymd("2005-12-31"), "foo", 2005L,
#'   1, 2, lubridate::ymd("2008-12-31"), "foo", 2008L,
#'   3, 3, lubridate::ymd("2007-12-31"), "foo", 2007L
#' )
#'
#' expand_prepost_positive(db)
#'
expand_prepost_positive <- function(db) {
  wide <- db |>
    dplyr::mutate(
      is_was_positive = TRUE
    ) |>
    tidyr::pivot_wider(
      ## WARNING: <= TRUE should be TRUE but is FASLE
      names_from = .data[["anno"]],
      values_from = .data[["is_was_positive"]],
      values_fill = FALSE
    )

  missing_years <- wide |>
    dplyr::select(dplyr::matches("^\\d{4}$")) |>
    names() |>
    setdiff(x = as.character(2004:2014))

  wide_compelte <- wide
  wide_compelte[missing_years] <- FALSE

  wide_compelte |>
    tidyr::pivot_longer(
      dplyr::matches("^\\d{4}$"),
      names_to = "anno",
      values_to = "is_first_positive",
      values_ptypes = integer()
    ) |>
    dplyr::group_by(.data[["Id_Medico"]], .data[["nPaz"]]) |>
    dplyr::mutate(anno = as.integer(.data[["anno"]])) |>
    dplyr::arrange(.data[["anno"]]) |>
    dplyr::mutate(
      is_was_positive = cumsum(.data[["is_first_positive"]]) |>
        as.logical()
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data[["is_first_positive"]]) |>
    dplyr::arrange(
      .data[["nPaz"]], .data[["Id_Medico"]], .data[["anno"]]
    )
}
