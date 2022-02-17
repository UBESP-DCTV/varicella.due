#' Create the main mixdb for the vericella project
#'
#' Create the main [mixdb] for the varicella project
#'
#' @param data_path (chr) path to the data folder
#' @param gold (chr) pedinet gold standard
#'
#' @return invisibly `TRUE``
#' @export
create_varicella_mixdb <- function(
    data_path = get_data_path(),
    gold
) {
  utils::globalVariables("where")

  gold |>
    dplyr::filter(!is.na(.data[["is_was_positive"]]))

  gold_varicella |>
    dplyr::mutate(
      dplyr::across(where(is.character), stringr::str_to_lower),
      dplyr::across(where(is.character), tidyr::replace_na, "__NA__"),
      dplyr::across(where(is.character), limpido::code_num),
      dplyr::across(where(is.character), limpido::expand_punctuations),
    ) |>
    limpido::mixdb(limpido::meta_vars(
      id_medico, n_paz, data, data_n, sesso, data_invio,
      vari_gold, anno, user_id, inizio_assistenza, fine_assistenza,
      data_elim, decesso_data, consenso_pedianet, min_of_data,
      max_of_data
    ))
}
