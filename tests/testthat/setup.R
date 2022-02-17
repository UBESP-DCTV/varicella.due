library(checkmate)

# data

if (basename(here::here()) == "varicella.due") {

  .tar_updated <- targets::tar_manifest(fields = "name")[["name"]] |>
    setdiff(targets::tar_outdated(targets_only = TRUE))

  tar_read_if_valid <- function(name) {
    if (!name %in% .tar_updated) {
      usethis::ui_warn("target {name} is outdated, beware of results.")
    }
    targets::tar_read_raw(name)
  }
}
