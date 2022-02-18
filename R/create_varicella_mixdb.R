#' Create the main mixdb for the vericella project
#'
#' Create the main [mixdb] for the varicella project
#'
#' @param gold_varicella (data frame) full varicella data to use,
#'   including a variable named `set` (with tag for "train" and
#'   "validation", and possibly "test" too), and a factor variable named
#'   `class` (with the class of interest, e.g. "negative" and
#'   "positive")
#'
#' @return invisibly `TRUE``
#' @export
create_varicella_mixdb <- function(gold_varicella) {
  utils::globalVariables("where")

  gold_varicella |>
    dplyr::mutate(
      dplyr::across(notes, stringr::str_to_lower),
      dplyr::across(notes, tidyr::replace_na, "__NA__"),
      dplyr::across(notes, limpido::code_num),
      dplyr::across(notes, limpido::expand_punctuations),
    ) |>
    limpido::mixdb(limpido::meta_vars(
      set, id_medico, n_paz, date, data_n, sesso, data_invio,
      vari_gold, anno, user_id, inizio_assistenza, fine_assistenza,
      data_elim, decesso_data, consenso_pedianet, min_of_data,
      max_of_data, giorni, class
    ))
}
