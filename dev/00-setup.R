
# Pacchetti -------------------------------------------------------


dev_pkgs <- c(
  "checkmate", "devtools", "fs", "gitcreds", "here", "markdown",
  "miniUI", "purrr", "stringr", "testthat", "usethis", "withr",
  "spelling", "lintr"
)


renv::install(dev_pkgs)

usethis::use_description(check_name = FALSE)
usethis::use_proprietary_license("Corrado Lanera & UBEP")


dev_pkgs |>
  purrr::walk(usethis::use_package, type = "Suggests")



meta_pkgs <- c("tidymodels", "tidyverse")
renv::install(meta_pkgs)


prj_pkgs <- c(
)
renv::install(prj_pkgs)
prj_pkgs |>
  purrr::walk(usethis::use_package)



gh_dev_pkgs <- c(
  "ropensci/targets",
  "ropensci/tarchetypes",
  "CorradoLanera/autotestthat"
)
renv::install(gh_dev_pkgs)

purrr::walk(gh_dev_pkgs, ~{
  package_name <- stringr::str_extract(.x, "[\\w\\.]+$")
  package_name |>
    usethis::use_dev_package(type = "Suggests", remote = .x)
})



# infrastruttura --------------------------------------------------

usethis::use_readme_rmd()
usethis::use_code_of_conduct("corrado.lanera@ubep.unipd.it")
usethis::use_lifecycle_badge("experimental")

usethis::git_vaccinate()

usethis::use_spell_check()


usethis::use_testthat()
# add # library(checkmate)
fs::file_create(here::here("tests/testthat/setup.R"))
usethis::use_test("setup")

usethis::use_r("utils")

usethis::edit_r_profile("project")



# finalization and isolation --------------------------------------

usethis::use_tidy_description()
renv::status()
renv::snapshot()





