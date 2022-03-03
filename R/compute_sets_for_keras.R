compute_sets_for_keras <- function(mixdb, max_words, max_len) {

  sets <- attr(mixdb, "meta")$set

  current_trainval_indeces <- compute_trainval_indeces(
    sets, validation_len = Inf, is_test = FALSE
  )

  final_traintest_indeces <- compute_trainval_indeces(
    sets, validation_len = 0L, is_test = TRUE
  )

  list(
    train = list(
      # return metrics on training too
      train = compute_training(
        mixdb, max_words, current_trainval_indeces, maxlen = max_len
      ),
      # return metrics on validation too
      validation  = compute_validation(
        mixdb, max_words, current_trainval_indeces, maxlen = max_len
      )
    ),

    test = list(
      # return metrics on training too
      train = compute_training(
        mixdb, max_words, final_traintest_indeces, maxlen = max_len
      ),
      # return metrics on validation too
      test  = compute_validation(
        mixdb, max_words, final_traintest_indeces, maxlen = max_len
      )
    )
  )
}
