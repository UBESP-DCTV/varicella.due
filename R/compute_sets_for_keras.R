compute_sets_for_keras <- function(mixdb, max_words, max_len) {

  sets <- attr(mixdb, "meta")$set

  current_trainval_indeces <- compute_trainval_indeces(
    sets, validation_len = 200L, is_test = FALSE
  )

  final_traintest_indeces <- compute_trainval_indeces(
    sets, validation_len = 0L, is_test = TRUE
  )

  list(
    train = list(
      train = compute_training(  # return metrics on training too
        mixdb, max_words, current_trainval_indeces, maxlen = max_len
      ),
      validation  = compute_validation(  # return metrics on validation too
        mixdb, max_words, current_trainval_indeces, maxlen = max_len
      )
    ),

    test = list(
      train = compute_training(  # return metrics on training too
        mixdb, max_words, final_traintest_indeces, maxlen = max_len
      ),
      test  = compute_validation(  # return metrics on validation too
        mixdb, max_words, final_traintest_indeces, maxlen = max_len
      )
    )
  )
}
