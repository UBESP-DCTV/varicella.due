# Targets setup

targets::tar_script()


# packages --------------------------------------------------------

prj_pkgs <- c(
  "tibble", "readr", "dplyr", "stringr", "lubridate", "tidyr", "furrr",
  "keras", "depigner", "janitor", "forcats", "future", "future.callr",
  "future.batchtools", "bs4Dash", "DT", "gt", "pingr", "shinybusy",
  "shinyWidgets", "qs", "clustermq"
)
renv::install(prj_pkgs)
prj_pkgs |>
  purrr::walk(usethis::use_package)


gh_dev_pkgs <- c(
  "UBESP-DCTV/limpido"
)
renv::install(gh_dev_pkgs)

purrr::walk(gh_dev_pkgs, ~{
  package_name <- stringr::str_extract(.x, "[\\w\\.]+$")
  package_name |>
    usethis::use_dev_package(remote = .x)
})


renv::status()
renv::snapshot()

# global setup ---------------------------------------------------

usethis::edit_r_environ("project")
usethis::use_tibble()

# Functions development

usethis::use_test("get_data_path")
usethis::use_r("get_data_path")


usethis::use_test("read_varic")
usethis::use_r("read_varic")

usethis::use_test("or_null")

usethis::use_test("eval_dates_for_gold")
usethis::use_r("eval_dates_for_gold")


usethis::use_test("expand_prepost_positive")
usethis::use_r("expand_prepost_positive")

basename(usethis::use_test("merge_records")) |>
  usethis::use_r()

basename(usethis::use_test("merge_id_records")) |>
  usethis::use_r()

basename(usethis::use_test("compose_trvaltest_up_to_year")) |>
  usethis::use_r()

basename(usethis::use_test("create_varicella")) |>
  usethis::use_r()

basename(usethis::use_test("meta_vars")) |>
  usethis::use_r()

basename(usethis::use_test("setup_input_data")) |>
  usethis::use_r()


basename(usethis::use_test("compute_trainval_indeces")) |>
  usethis::use_r()

basename(usethis::use_test("compute_training")) |>
  usethis::use_r()

basename(usethis::use_test("compute_validation")) |>
  usethis::use_r()

usethis::use_r("compute_sets_for_keras")


basename(usethis::use_test("get_current_parameter")) |>
  usethis::use_r()

usethis::use_r("train_simple_embedding")

usethis::use_r("gg_history")

# Dev cycle -------------------------------------------------------

spelling::spell_check_package()
# spelling::update_wordlist()
lintr::lint_package()


# CTRL + SHIFT + D: update project documentation
# CTRL + SHIFT + T: run all project's tests
# CTRL + SHIFT + E: run all CRAN tests



# Targets cycle ---------------------------------------------------

source(here::here("run.R"))
