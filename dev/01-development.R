# Targets setup

targets::tar_script()



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


# Dev cycle -------------------------------------------------------

spelling::spell_check_package()
# spelling::update_wordlist()
lintr::lint_package()


# CTRL + SHIFT + D: update project documentation
# CTRL + SHIFT + T: run all project's tests
# CTRL + SHIFT + E: run all CRAN tests



# Targets cycle ---------------------------------------------------

source(here::here("run.R"))
targets::tar_visnetwork()
