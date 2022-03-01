#' Title
#'
#' @param varic_df
#' @param popolazione
#' @param gold
#'
#' @return
#' @export
#'
#' @examples
create_varicella <- function(varic_df, popolazione, gold) {
  varic_df |>
    dplyr::left_join(popolazione, by = c("id_medico", "n_paz")) |>
    dplyr::left_join(gold, by = c("n_paz", "anno")) |>
    dplyr::group_by(.data[["n_paz"]], .data[["anno"]]) |>
    dplyr::mutate(class = dplyr::if_else(
      "positive" %in% .data[["class"]], "positive", "negative"
    )) |>
    dplyr::ungroup()
}
