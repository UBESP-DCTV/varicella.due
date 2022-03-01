compute_training <- function(mixdb, max_words, trainval_indeces, maxlen = 4000L) {
  train_indeces <- trainval_indeces[["train_indeces"]]

  train_x <- mixdb$x[train_indeces] %>%
    limpido::add_oov_when_greater_than(max_words)
  train_lens <- purrr::map_int(train_x, length)
  train_dist <- quantile(train_lens, c(.5, .75, .90, .95, .99))
  train_mean_len <- mean(train_lens)
  train_x  <- train_x %>%
    keras::pad_sequences(
      maxlen, padding = "post", truncating = "post",
      # 1:max_word sono gli indici delle parole,
      # max_word + 1L è l'indice di __OOV__
      # mettiamo max_word + 2L come indice di __PAD__, ovvero una
      # parola che non c'è!!
      value = max_words + 2L
    )

  train_y <- as.integer(mixdb$y[train_indeces]) - 1L
  names(train_y) <- rep("train", length(train_y))
  train_y <- keras::to_categorical(train_y)
  n_class <- ncol(train_y)
  usethis::ui_done("Training set ready")

  list(
    train_x = train_x - 1L,
    train_y = train_y,
    train_indeces = train_indeces,
    train_dist = train_dist,
    train_mean_len = train_mean_len,
    train_len = nrow(train_x),
    n_class = n_class
  )
}
