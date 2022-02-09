library(targets)
library(tarchetypes)
library(here)



list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()



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
    file.path(get_data_path(), "positive_gold.rdS"),
    format = "file"
  ),
  tar_target(gold, read_varic(rawGold, "positive_gold")),

  tar_target(
    rawDf,
    file.path(get_data_path(), "varic_df.rds"),
    format = "file"
  ),
  tar_target(varic_df, read_varic(rawDf, "varic_df")),

  tar_target(
    goldWithFirstDate,
    eval_dates_for_gold(gold, varic_df)
  ),



  # compile the report
  tar_render(report, here("reports/report.Rmd"))
)
