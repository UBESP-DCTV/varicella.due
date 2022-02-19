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

  tar_target(
    maxWords,
    min(
      length(limpido:::get_dictionary(mixdbs)),
      122607L # all words in the pretrained
    ),
    pattern = map(mixdbs),
    iteration = "list"
  ),

  tar_target(embeddingDim, "300"), # or "100"
  tar_target(fasttextPretrained, {
    pretrained_path <- embeddingDim |>
      switch(
        "100" = file.path(get_data_path(), "model_100.vec"),
        "300" = file.path(get_data_path(), "model_300.vec")
      )
    readr::read_lines(pretrained_path, skip = 1L)
  }),

  tar_target(
    embeddingMatrix,
    limpido:::embedding_mtrx(
      mixdbs, fasttextPretrained, embeddingDim, maxWords
    ),
    pattern = map(mixdbs, maxWords),
    iteration = "list"
  ),

  tar_target(
    setupInputData,
    setup_input_data(
      mixdb = mixdbs,
      embedding_matrix = embeddingMatrix,
      fasttext_pretrained = fasttextPretrained,
      max_words = maxWords,
      embedding_dim = embeddingDim
    ),
    pattern = map(mixdbs, maxWords, embeddingMatrix),
    iteration = "list"
  ),


  # compile the report
  tar_render(report, here::here("reports/report.Rmd"))
)
