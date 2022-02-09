#' Read varicella2 DBs
#'
#' @param what (chr) one of "popolazione", "positive_gold", "varic_df"
#'
#' @return a [tibble][tibble::tibble-package] for the corresponding
#'  [what] dataset.
#' @export
#'
#' @examples
#' read_varic("popolazione")
#' read_varic("positive_gold")
#' read_varic("varic_df")
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
  readr::read_rds(path)
}
read_gold <- function(path) {
  readr::read_rds(path)
}
read_varic_df <- function(path) {
  readr::read_rds(path)
}
