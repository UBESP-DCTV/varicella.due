#' Varicella meta-data variables
#'
#' @return (chr) meta-data variable names
#' @export
#'
#' @examples
#' meta_vars()
meta_vars <- function() {
  c("id_medico", "date", "data_n", "sesso", "data_invio",
    "vari_gold", "user_id", "inizio_assistenza", "fine_assistenza",
    "data_elim", "decesso_data", "consenso_pedianet", "min_of_data",
    "max_of_data", "giorni")
}
