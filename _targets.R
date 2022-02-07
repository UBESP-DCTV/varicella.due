library(targets)
library(tarchetypes)
library(here)

list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Set target-specific options such as packages.
tar_option_set(
  packages = c("here")#, error = "continue"
)

# End this file with a list of target objects.
list(

  tar_target(ciao, 1 + 1),
  # Call your custom functions as needed.
  # tar_target(
  #   rawPath,
  #   here("data-raw/Miocarditi_DATA_2021-11-29_0913.csv"),
  #   format = "file"
  # ),
  # tar_target(rawImported, import_raw(rawPath)),
  # tar_target(preprocessed, preprocess_raw(rawImported)),

  # compile the report
  tar_render(report, here("reports/report.Rmd"))
)
