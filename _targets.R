library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)


list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


tar_option_set(
  error = "abridge",
  packages = c("purrr", "here", "future")
)

# End this file with a list of target objects.
list(

  tar_target(rawPop, file.path(get_data_path(), "popolazione.rds"),
             format = "file"),
  tar_target(popolazione, read_varic(rawPop, "popolazione")),
  tar_target(rawGold, file.path(get_data_path(), "positive_gold.rds"),
             format = "file"),
  tar_target(globalGold, read_varic(rawGold, "positive_gold")),
  tar_target(rawDf, file.path(get_data_path(), "varic_df.rds"),
             format = "file"),
  tar_target(varic_df, read_varic(rawDf, "varic_df")),

  tar_target(goldDate, eval_dates_for_gold(globalGold, varic_df)),
  tar_target(gold, expand_prepost_positive(goldDate, popolazione)),
  tar_target(varicella, create_varicella(varic_df, popolazione, gold)),

  tar_target(last_year_of_data, 2004:2005),

  tar_target(
    varicellas,
    compose_trvaltest_up_to_year(varicella, last_year_of_data),
    pattern = map(last_year_of_data),
    iteration = "list"
  ),
  tar_target(
    ready2mixes,
    merge_text_meta_records(varicellas, meta_vars()),
    pattern = map(varicellas),
    iteration = "list"
  ),
  tar_target(
    mixdbs,
    create_varicella_mixdb(ready2mixes),
    pattern = map(ready2mixes),
    iteration = "list"
  ),




  # compile the report
  tar_render(report, here::here("reports/report.Rmd"))
)
