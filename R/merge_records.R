#' Merge patient's records
#'
#' Merge together all the record from a patient's ID (in a year), by
#' separating each field by an "__EOF__" token, and each observation
#' by an "__EOR__" one.
#'
#' @param x (data frame) dataframe of data and metadata for a single
#'  ID, for a single year (and a set, and all other supposed unique
#'  variables for that ID-year group)
#'
#' @param meta_vars (chr) included variables in `x` that should not be
#'   merged as a text. The will be summarized as a single list-entry in
#'   the final one-row dataframe
#'
#' @return a dataframe with a single row
#' @export
#'
#' @examples
#'
#' db <- tibble::tribble(
#'   ~txt_1, ~txt_2, ~meta_txt, ~ meta_num,
#'    "faa tra", "bar", "tar", 12,
#'    "fee fir", NA, "ter", NA,
#'    NA, "bir", "tir", 17,
#'    "foo", "bor", NA, 24,
#'    "fuu", "bur", "tur", 82
#' ) |>
#'   dplyr::mutate(date = sample(1:5 + Sys.Date()))
#'
#' meta_vars <- c("meta_txt", "meta_num", "date")
#'
#' merge_records(db, meta_vars) |>
#'   dplyr::glimpse()
#'
#'
merge_records <- function(db, meta_vars) {

  meta <- dplyr::intersect(meta_vars, names(db))

  # Separated process because C stack usage too close to the limit
  db |>
    dplyr::arrange(.data[["date"]]) |>
    dplyr::mutate(
      dplyr::across(-dplyr::all_of(meta), stringr::str_to_lower),
      dplyr::across(-dplyr::all_of(meta), tidyr::replace_na, "__NA__")
    ) |>
    tidyr::unite(notes, -dplyr::all_of(meta), sep = " __EOF__ ") |>
    dplyr::mutate(
      notes = purrr::map_chr(.data[["notes"]], add_endofrecord)
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(meta), list),
      notes = paste(.data[["notes"]], collapse = " ")
    )
}
