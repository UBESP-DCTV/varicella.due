compute_validation <- function(
    mixdb, max_words, trainval_indeces, maxlen
) {
  validation_indeces <- trainval_indeces[["train_indeces"]]

  validation_x <- mixdb$x[validation_indeces] %>%
    limpido::add_oov_when_greater_than(max_words)
  validation_lens <- purrr::map_int(validation_x, length)
  validation_dist <- quantile(validation_lens, c(.5, .75, .90, .95, .99))
  validation_mean_len <- mean(validation_lens)
  validation_x <- validation_x %>%
    keras::pad_sequences(maxlen,
                         padding = "post", truncating = "post",
                         value = max_words + 2L
    )

  validation_y <- as.integer(mixdb$y[validation_indeces]) - 1L
  names(validation_y) <- rep("validation", length(validation_y))
  validation_y <- keras::to_categorical(validation_y)
  usethis::ui_done("Validation set ready")

  if (trainval_indeces[["is_test"]]) {
    usethis::ui_warn(
      "Remember that now the 'validation' set is the test set!!"
    )
  }


  list(
    validation_x = validation_x - 1L,
    validation_len = nrow(validation_x),
    validation_y = validation_y,
    validation_indeces = validation_indeces,
    validation_dist = validation_dist,
    validation_mean_len = validation_mean_len
  )
}
