#' Merge records by id and year
#'
#' Merge textual and meta information for each id by year.
#' - All unique, non textual, information are kept as they are
#' - Each meta information is merged in a single row, concatenated in
#'   a global column-list (in the final data frame.
#' - All textual information are merged in a single row and in a single
#'   column. Each field separated by a "__EOF__" token, each record
#'   separated by a "__EOR__" token.
#'
#'  `db` must have column `set`
#'  ("train", "validation", adn possibly "test"); `anno` (the year
#'   considered); `n_paz` (patient id); `class` ("positive", "negative")
#'
#' @param db data frame
#'
#' @return a data frame with a single row for each combination of
#'   id-year
#' @export
#'
#' @examples
#'
#'
#' db <- tibble::tribble(
#'   ~id_medico, ~n_paz, ~anno, ~class, ~set, ~txt_1, ~txt_2, ~meta_txt, ~ meta_num,
#'    1, 1, 2005, "positive", "train", "faa tra", "bar", "tar", 12,
#'    2, 1, 2006, "negative", "validation", "fee fir", NA, "ter", NA,
#'    1, 2, 2008, "negative", "train", NA, "bir", "tir", 17,
#'    1, 3, 2006, "positive", "train", "foo", "bor", NA, 24,
#'
#'    # patient 4 has two id_medico qithin same year and set with
#'    # different class. "positive class" will predominate
#'    3, 4, 2004, "positive", "train", "fuu", "bur", "tur", 82,
#'    4, 4, 2004, "negative", "train", "fuu", "bur", "tur", 82
#' ) |>
#'   dplyr::mutate(date = rev(1:6 + Sys.Date()))
#'
#' meta_vars <- c("id_medico", "meta_txt", "meta_num", "date")
#'
#' res <- merge_text_meta_records(db, meta_vars)
#' res
#'
merge_text_meta_records <- function(db, meta_vars = character()) {
  utils::globalVariables("where")

  aux <- db |>
    dplyr::group_by(anno, n_paz) |>
    dplyr::mutate(
      class = dplyr::if_else(
        "positive" %in% .data[["class"]],
        "positive",
        "negative"
      )
    ) |>
    tidyr::nest(
      data = -c(
        set, anno, n_paz, class,
        where(~!is.character(.x) & length(unique(.x)) == 1L),

      )
    ) |>
    dplyr::ungroup()

  aux_names <- setdiff(names(aux), "data")

  aux |>
    dplyr::mutate(
      data = data |>
        purrr::map(
          merge_records,
          meta_vars = setdiff(meta_vars, aux_names)
        )
    ) |>
    tidyr::unnest(cols = c(data))
}
