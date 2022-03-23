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

  gold_varicella[["notes"]] <- unlist(gold_varicella[["notes"]])
  gold_varicella[["notes"]] <- limpido::code_num(
    gold_varicella[["notes"]]
  )
  gold_varicella[["notes"]] <- limpido::expand_punctuations(
    gold_varicella[["notes"]]
  )

  limpido::mixdb(
    gold_varicella,
    limpido::meta_vars(
      set, id_medico, n_paz, date, data_n, sesso, data_invio,
      vari_gold, anno, user_id, inizio_assistenza, fine_assistenza,
      data_elim, decesso_data, consenso_pedianet, min_of_data,
      max_of_data, giorni, class
    )
  )
}
