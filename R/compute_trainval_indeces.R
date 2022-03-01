compute_trainval_indeces <- function(corpus_sets, validation_len, is_test) {

  all_train_indeces <- which(corpus_sets == "train")
  all_validation_indeces <- which(corpus_sets == "validation")
  all_test_indeces <- which(corpus_sets == "test")

  validation_indeces <- sample(
    all_validation_indeces,
    min(validation_len, length(all_validation_indeces))
  )

  train_vali_indeces <- setdiff(
    all_validation_indeces,
    validation_indeces
  )
  train_indeces <-  c(all_train_indeces, train_vali_indeces)
  train_indeces <- sample(train_indeces)

  if (is_test) {
    validation_indeces <- all_test_indeces
    usethis::ui_warn("Test set has taken the place of the validation set!!")
  }

  list(
    train_indeces = train_indeces,
    validation_indeces = validation_indeces,
    is_test = is_test
  )
}
