options(tidyverse.quiet = TRUE)

library(targets)
library(tarchetypes)
library(tibble)


list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


tar_option_set(
  error = "abridge",
  format = "qs",
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")
  ),
  garbage_collection = TRUE,
  memory = "transient"
)


values <- tibble(last_training_year = 2004:2013)


targets <- tar_map(
  values = values,
  tar_target(
    varicellas,
    compose_trvaltest_up_to_year(varicella, last_training_year)
  ),
  tar_target(
    ready2mixes,
    merge_text_meta_records(varicellas, meta_vars())
  ),

  # Corpus and like -----------------------------------------------
  tar_target(
    mixdbs,
    create_varicella_mixdb(ready2mixes)
  ),


  tar_target(
    corpusDictionary,
    limpido::get_dictionary(mixdbs)
  ),
  tar_target(
    corpusDicSize,
    length(corpusDictionary)
  ),
  tar_target(
    maxWords,
    min(corpusDicSize, 122607L)  # all words in pedianet pretrained
  ),

  tar_target(
    embeddingMatrix,
    limpido:::embedding_mtrx(
      mixdbs, fasttextPretrained, embeddingDim, maxWords
    )
  ),
  # Train/Validation/Test -----------------------------------------
  tar_target(
    sets_for_keras,
    compute_sets_for_keras(mixdbs, maxWords, maxLen)
  ),




  # Parameters ----------------------------------------------------
  tar_target(adjustesMaxWords, maxWords + 2),


  # Keras -----------------------------------------------------------
  tar_target(
    testSimpleEmbedding,
    train_simple_embedding(
      # embeddings
      maxLen, adjustesMaxWords,
      embeddingDim, embeddingMatrix,
      pediaDictSize, corpusDicSize,

      # train
      inputDO, layersDO, preoutputFCunits,

      # optimize
      loss, optimizer, metrics,

      # fit
      sets_for_keras, batchSize, epochs,

      # log
      last_year_of_data = last_training_year,
      use_weights = TRUE,
      is_test = FALSE,
      save_model = TRUE,
      tg = TRUE,
      keras_verbose = 0
    ),
    packages = c("keras", "depigner")
  )

)


# End this file with a list of target objects.
list(

  # raw source files ----------------------------------------------
  tar_target(
    rawPop,
    file.path(get_data_path(), "popolazione.rds"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    popolazione,
    read_varic(rawPop, "popolazione"),
    deployment = "main"
  ),


  tar_target(
    rawGold,
    file.path(get_data_path(), "positive_gold.rds"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    globalGold,
    read_varic(rawGold, "positive_gold"),
    deployment = "main"
  ),


  tar_target(
    rawDf,
    file.path(get_data_path(), "varic_df.rds"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    varic_df,
    read_varic(rawDf, "varic_df"),
    deployment = "main"
  ),




  # Main dataset(s) -----------------------------------------------
  tar_target(
    goldDate,
    eval_dates_for_gold(globalGold, varic_df),
    deployment = "main"
  ),
  tar_target(
    gold,
    expand_prepost_positive(goldDate, popolazione),
    deployment = "main"
  ),


  tar_target(
    varicella,
    create_varicella(varic_df, popolazione, gold),
    deployment = "main"
  ),









  # Embeddings ----------------------------------------------------
  tar_target(
    maxLen,
    4000L,
    deployment = "main"
  ),
  tar_target(
    embeddingDim,
    "300",
    deployment = "main"
  ),


  tar_target(
    rawFT300,
    file.path(get_data_path(), "model_300.vec"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    fasttextPretrained300,
    readr::read_lines(rawFT300, skip = 1L),
    deployment = "main"
  ),


  tar_target(
    rawFT100,
    file.path(get_data_path(), "model_100.vec"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    fasttextPretrained100,
    readr::read_lines(rawFT100, skip = 1L),
    deployment = "main"
  ),


  tar_target(fasttextPretrained,
    switch(embeddingDim,
      "100" = fasttextPretrained100,
      "300" = fasttextPretrained300
    ),
    deployment = "main"
  ),

  tar_target(pediaDictSize,
    length(fasttextPretrained),
    deployment = "main"
  ),







  # tar_target(
  #   requirements, {
  #
  #     varicella_up_to_year <- varicella |>
  #       compose_trvaltest_up_to_year(last_year_of_data)
  #     rm(varicella); gc(FALSE)
  #
  #     merged <- merge_text_meta_records(
  #       varicella_up_to_year, meta_vars()
  #     )
  #     rm(varicella_up_to_year); gc(FALSE)
  #
  #     mixdb <- create_varicella_mixdb(merged)
  #     rm(merged); gc(FALSE)
  #
  #     corpus_dict_size <- limpido::get_dictionary(mixdb) |>
  #       length()
  #
  #     max_words = min(corpus_dict_size, 122607L)
  #
  #     embedding_matrix <- limpido:::embedding_mtrx(
  #       mixdb, fasttextPretrained, embeddingDim, max_words
  #     )
  #
  #     sets_for_keras <- compute_sets_for_keras(mixdb, max_words, maxLen)
  #
  #     rm(mixdb); gc(FALSE)
  #
  #     list(
  #       corpus_dict_size = corpus_dict_size,
  #       embedding_matrix = embedding_matrix,
  #       sets_for_keras = sets_for_keras,
  #       adjustesMaxWords = max_words + 2
  #     )
  #   },
  #   pattern = map(last_year_of_data),
  #   iteration = "list",
  #   deployment = "worker",
  #   storage = "worker",
  #   memory = "transient"
  # ),
  #



  tar_target(
    inputDO,
    0.2,
    deployment = "main"
  ),
  tar_target(
    layersDO,
    0.5,
    deployment = "main"
  ),
  tar_target(
    preoutputFCunits,
    128L,
    deployment = "main"
  ),

  tar_target(
    loss,
    "binary_crossentropy",
    deployment = "main"
  ),
  tar_target(
    optimizer,
    "adam",
    deployment = "main"
  ),
  tar_target(
    metrics,
    "accuracy",
    deployment = "main"
  ),

  tar_target(
    batchSize,
    8L,
    deployment = "main"
  ),
  tar_target(
    epochs,
    50L,
    deployment = "main"
  ),


  # Keras -----------------------------------------------------------

  targets,


  # Report ----------------------------------------------------------
  tar_render(report, here::here("reports/report.Rmd"))
)
