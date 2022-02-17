library(targets)
library(tarchetypes)
library(here)



list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


tar_option_set(
  error = "abridge"
)

# End this file with a list of target objects.
list(

  tar_target(
    rawPopolazione,
    file.path(get_data_path(), "popolazione.rds"),
    format = "file"
  ),
  tar_target(popolazione, read_varic(rawPopolazione, "popolazione")),


  tar_target(
    rawGold,
    file.path(get_data_path(), "positive_gold.rds"),
    format = "file"
  ),
  tar_target(globalGold, read_varic(rawGold, "positive_gold")),

  tar_target(
    rawDf,
    file.path(get_data_path(), "varic_df.rds"),
    format = "file"
  ),
  tar_target(varic_df, read_varic(rawDf, "varic_df")),

  tar_target(
    goldDate,
    eval_dates_for_gold(globalGold, varic_df)
  ),

  tar_target(
    gold,
    expand_prepost_positive(goldDate, popolazione)
  ),

  tar_target(
    varicella,
    varic_df |>
      dplyr::left_join(popolazione) |>
      dplyr::left_join(gold)
  ),

  # #
  # tar_target(
  #   mixdb_list,
  #   dplyr::left_join(varic_df, gold) |>
  #     create_varicella_mixdb()
  # ),


  # compile the report
  tar_render(report, here("reports/report.Rmd"))
)
