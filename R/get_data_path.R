#' Path to data
#'
#' @return (chr) the path to the data folder
#' @export
#'
#' @examples
#' \dontrun{
#'   popolazione <- file.path(data_path, "popolazione.rda")
#'   popolazione
#' }
get_data_path <- function() {
  data_path <- Sys.getenv("DATA_PATH")

  if (data_path == "") {
    usethis::ui_stop(
      "Environmental variable {usethis::ui_field('DATA_PATH')} must be set.
      It should point to your <varicella 2 source data folder>.
      You can set it adding a {usethis::ui_value('DATA_PATH=<your/path/here>')} entry inside the project {usethis::ui_field('.Renviron')}
      To open the project {usethis::ui_field('.Renviron')} you can call{usethis::ui_code('usethis::edit_r_environ(\"project\")')}"
    )
  }

  data_path

}
