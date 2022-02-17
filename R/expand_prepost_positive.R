#' Expand previous and post positive status
#'
#' Given a dataset with columns `id_medico`, `n_paz`, and `data`
#' (the latter referred to the year of Varicella infection), it adds
#' rows for each combination of `id_medico` and `n_paz` reporting the
#' status of "they was infected" for all the years from 2004 to 2014
#' in a column named `class`
#'
#' @param db_gold (data frame) positive database with `id_medico`, `n_paz`,
#' and `data` as the minimum set of columns. Column `data` reporting the
#' year of the Varicella infection for the corresponding patient.
#' @param pop (data frame) database with all population IDs stored in a
#'  `n_paz` variable with corresponding `id_medico` for each one.
#'
#' @return the original [tibble][tibble::tibble-package] with the
#' `class` column added as last one.
#' @export
#'
#' @examples
#' db_gold <- tibble::tribble(
#'   ~ id_medico, ~n_paz, ~anno, ~class,
#'   1, 1, 2005L, "is_was_positive",
#'   2, 1, 2005L, "is_was_positive",
#'   1, 2, 2008L, "is_was_positive",
#'   3, 3, 2007L, "is_was_positive"
#' )
#'
#' pop <- data.frame(
#'   id_medico = c(1, 2, 1, 3, 3),
#'   n_paz = c(1, 1, 2, 3, 4),
#'   foo = "bar"
#' )
#'
#' expand_prepost_positive(db_gold, pop)
#'
expand_prepost_positive <- function(db_gold, pop) {

  pop <- pop |>
    dplyr::select(dplyr::any_of(names(db_gold)))

  wide <- db_gold |>
    tidyr::pivot_wider(
      names_from = .data[["anno"]],
      values_from = .data[["class"]],
      values_fill = "negative"
    ) |>
    dplyr::bind_rows(pop) |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("^\\d{4}$"), ~{
        dplyr::if_else(is.na(.x), "negative", .x)
      })
    )

  missing_years <- wide |>
    dplyr::select(dplyr::matches("^\\d{4}$")) |>
    names() |>
    setdiff(x = as.character(2004:2014))

  wide_compelte <- wide
  wide_compelte[missing_years] <- "negative"

  wide_compelte |>
    tidyr::pivot_longer(
      dplyr::matches("^\\d{4}$"),
      names_to = "anno",
      values_to = "is_first_positive",
      values_ptypes = character()
    ) |>
    dplyr::mutate(
      class = .data[["is_first_positive"]] |>
        factor(
          levels = c("negative", "is_was_positive"),
          labels = c("negative", "positive")
        ),
      anno = as.integer(.data[["anno"]])
    ) |>
    dplyr::select(-.data[["is_first_positive"]]) |>
    dplyr::arrange(
      .data[["n_paz"]], .data[["id_medico"]], .data[["anno"]]
    )
}
