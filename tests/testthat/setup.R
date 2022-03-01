library(checkmate)


# supporting functions --------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) {
    usethis::ui_info("{usethis::ui_code('x')} is NULL, default is used")
    y
  } else {
    x
  }
}

tar_read_if_valid <- function(name) {
  if (basename(here::here()) == "varicella.due") {
    if (!name %in% .tar_updated) {
      usethis::ui_warn("target {name} is outdated, beware of results.")
    }
    targets::tar_read_raw(name)
  } else {
    testthat::skip("not in varicella.due project")
  }
}


# data
if (basename(here::here()) == "varicella.due") {
  .tar_updated <- targets::tar_manifest(fields = "name")[["name"]] |>
    setdiff(targets::tar_outdated(targets_only = TRUE))
}
