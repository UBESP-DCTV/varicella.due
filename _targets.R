library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)


list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


tar_option_set(
  error = "abridge",
  packages = c("purrr", "here", "future"),
  format = "qs",
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")
  )
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

  tar_target(corpusDictionary, limpido::get_dictionary(mixdbs),
             pattern = map(mixdbs),
             iteration = "list" ),


  tar_target(corpusDicSize, length(corpusDictionary),
             pattern = map(corpusDictionary),
             iteration = "list"),

  tar_target(corpusSets, attr(mixdbs, "meta")$set,
             pattern = map(mixdbs),
             iteration = "list"),

  tar_target(
    maxWords,
    min(
      corpusDicSize,
      122607L # all words in the pretrained in pedianed
    ),
    pattern = map(corpusDicSize),
    iteration = "list"
  ),

  tar_target(maxLen, 4000L),

  tar_target(embeddingDim, "300"),

  tar_target(rawFT300, file.path(get_data_path(), "model_300.vec"),
             format = "file"),
  tar_target(fasttextPretrained300,
             readr::read_lines(rawFT300, skip = 1L)),

  tar_target(rawFT100, file.path(get_data_path(), "model_100.vec"),
             format = "file"),
  tar_target(fasttextPretrained100,
             readr::read_lines(rawFT100, skip = 1L)),


  tar_target(fasttextPretrained,
    switch(embeddingDim,
      "100" = fasttextPretrained100,
      "300" = fasttextPretrained300
    )
  ),



  tar_target(
    embeddingMatrix,
    limpido:::embedding_mtrx(
      mixdbs, fasttextPretrained, embeddingDim, maxWords
    ),
    pattern = map(mixdbs, maxWords),
    iteration = "list"
  ),

  tar_target(
    currentTrainvalIndeces,
    compute_trainval_indeces(
      corpusSets, validation_len = 200L, is_test = FALSE
    ),
    pattern = map(corpusSets),
    iteration = "list"
  ),

  tar_target(
    currentTrain,
    compute_training(
      mixdbs, maxWords, currentTrainvalIndeces, maxlen = maxLen
    ),
    pattern = map(mixdbs, maxWords, currentTrainvalIndeces),
    iteration = "list"
  ),

  tar_target(
    currentValidation,
    compute_validation(
      mixdbs, maxWords, currentTrainvalIndeces, maxlen = maxLen
    ),
    pattern = map(mixdbs, maxWords, currentTrainvalIndeces),
    iteration = "list"
  ),

  tar_target(currentParameters,
    get_current_parameter(
      max_words = maxWords, # this will updated after OOV
      batch_size = 8L,
      epochs = 15L,
      loss      = "categorical_crossentropy",
      metrics   = "categorical_accuracy",
      optimizer = "adam"
    ),
    pattern = map(maxWords),
    iteration = "list"
  ),


  # compile the report
  tar_render(report, here::here("reports/report.Rmd"))
)
