#' Read varicella2 DBs
#'
#' @param path (chr) path to RDS to import
#' @param db (chr) one of  "popolazione", "positive_gold", or
#'   "varic_df". Note that this is a mandatory field.
#'
#' @return a [tibble][tibble::tibble-package] for the corresponding
#'  `path` dataset.
#' @export
#'
#' @examples
#' \dontrun{
#'   read_varic(get_data_path(), "popolazione")
#'   read_varic(get_data_path(), "positive_gold")
#'   read_varic(get_data_path(), "varic_df")
#' }
#'
read_varic <- function(
  path,
  db = c(NULL, "popolazione", "positive_gold", "varic_df")
) {
  stopifnot(file.exists(path))
  stopifnot(!is.null(db))
  db <- match.arg(db)


  switch(db,
    "popolazione" = read_popolazione(path),
    "positive_gold" = read_gold(path),
    "varic_df" = read_varic_df(path),
     usethis::ui_stop(
      "Very strange error, should never be signaled"
    )
  )
}



read_popolazione <- function(path) {
  readr::read_rds(path) |>
    janitor::clean_names() |>
    dplyr::distinct()

}
read_gold <- function(path) {
  readr::read_rds(path) |>
    janitor::clean_names() |>
    dplyr::distinct()
}
read_varic_df <- function(path) {
  readr::read_rds(path) |>
    janitor::clean_names() |>
    dplyr::distinct()
}
