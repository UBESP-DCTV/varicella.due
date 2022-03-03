train_bidirectional_gru <- function(
    # empbeddings
    max_len,
    max_words,  # adjusted for OOV and PAD!!
    embedding_dim,
    embedding_matrix,
    pedia_dict_size,
    corpus_dict_size,

    # train
    input_do,
    layers_do,
    preoutput_fc_units,

    # optimize
    loss,
    optimizer,
    metrics,

    # fit
    sets_for_keras,
    batch_size,
    epochs,

    # log
    last_year_of_data,
    use_weights = TRUE,
    is_test = FALSE,
    save_model = TRUE,
    tg = TRUE,
    keras_verbose = 0  # for {future} workers
) {

  tg <- tg && Sys.getenv("R_telegram_bot_name") != ""
  if (tg) start_bot_for_chat("varicella.due")


  # Data ============================================================
  sets <- if (is_test) sets_for_keras[["test"]] else sets_for_keras[["train"]]
  train <- sets[["train"]]
  validation <- if (is_test) sets[["test"]] else sets[["validation"]]
  rm(sets)
  gc(FALSE)

  weights <- if (use_weights) {
    as.list(nrow(train$train_y)/(train$n_class * colSums(train$train_y))) |>
      setNames(c("0", "1"))
  } else {
    list("0" = 1, "1" = 1)
  }

  # Model definition ================================================
  run_name <- glue::glue("0.1.1.{last_year_of_data}-bidirectional-GRU")
  architecture <- glue::glue("
  ```
  ARCHITECTURE
  ------------

  name      : {run_name}
  time      : {lubridate::now()}
  train year: {last_year_of_data}
  achitect  : CL
  network   : bi-GRU{2*preoutput_fc_units} + 2FC{preoutput_fc_units}
  ```
  ")
  if (tg) {
    depigner::send_to_telegram(glue::glue("start {run_name}"))
    depigner::send_to_telegram(architecture, parse_mode = "Markdown")
  }

  # Layer 1 =========================================================
  # l1_input --------------------------------------------------------
  l1_input <- keras::layer_input(shape = c(max_len))

  l1_embedding <- l1_input |>
    keras::layer_embedding(
      input_dim = max_words,
      output_dim = embedding_dim,
      input_length = max_len,
      trainable = FALSE,
      weights = list(embedding_matrix),
      name = "l1_embedding"
    ) |>
    # [batch, words, embeddings] --> [batch, max-embeddings]
    keras::layer_global_max_pooling_1d() |>
    keras::layer_batch_normalization(name = "l1_batch-norm") |>
    keras::layer_dropout(input_do, name = "l1_dropout")

  # l2_gru
  l2_gru <- l1_embedding |>
    keras::bidirectional(keras::layer_gru(
      units = 2 * preoutput_fc_units,
      dropout = layers_do
    ))



  # OUTPUT ==========================================================
  output <- l2_gru |>
    keras::layer_dense(
      units = preoutput_fc_units,
      activation = "relu",
      name = "fc_out_1"
    ) |>
    keras::layer_batch_normalization() |>
    keras::layer_dropout(layers_do) |>
    keras::layer_dense(
      units = preoutput_fc_units,
      activation = "relu",
      name = "fc_out_2"
    ) |>
    keras::layer_batch_normalization() |>
    keras::layer_dropout(layers_do) |>
    keras::layer_dense(
      units = 2L,
      activation = "sigmoid",
      name = "out_fc")


  # MODEL ===========================================================
  model <- keras::keras_model(l1_input, output)

  if (keras_verbose > 0) summary(model)

  # COMPILE =========================================================
  model |>
    keras::compile(
      loss      = loss,
      optimizer = optimizer,
      metrics   = metrics
    )

  # Run =============================================================
  {
    start_time <- lubridate::now()

    history <- keras::fit(
      model,
      # train ------------------------
      x = train[["train_x"]],
      y = train[["train_y"]],
      class_weight = weights,
      # validation -------------------
      validation_data = list(
        validation[["validation_x"]],
        validation[["validation_y"]]
      ),
      # learning pace ----------------
      batch_size = batch_size,
      epochs     = epochs,

      # targets requirements
      verbose = keras_verbose
    )

    train_time <- lubridate::now() - start_time
  }

  if (tg) {
    glue::glue("
      ```
      {run_name} trained
      duration : {lubridate::as.duration(round(as.numeric(train_time)))}
      ```
    ") |>
      depigner::send_to_telegram()
  }


  # Plot and save ===================================================
  # plot(history) not in gg_history b/c keras'plot() is not exported!

  params <- list(
    run_name = run_name,
    architecture = architecture,
    is_test = is_test,
    train = train,
    validation = validation,
    start_time = start_time,
    train_time = train_time,
    pedia_dict_size = pedia_dict_size,
    corpus_dict_size = corpus_dict_size,
    max_words = max_words,
    embedding_dim = embedding_dim,
    max_len = max_len,
    batch_size = batch_size,
    epochs = epochs,
    loss = loss,
    optimizer = optimizer,
    metrics = metrics,
    history = history
  )

  p <- plot(history) |>
    gg_history(history, params)

  if (tg) depigner::send_to_telegram(p)

  list(
    serialized_model = if (save_model) keras::serialize_model(model) else NULL,
    params = params
  )
}
